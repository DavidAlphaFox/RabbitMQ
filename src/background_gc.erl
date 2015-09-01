%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%
%% RabbitMQ做为一个消息队列，很多时候需要消耗大量的内存
%% 适当的时候，就要手动的进行GC
%% 这样才能保证整个队列的性能平稳
%% 我们可以想像一下，如果队列处在高负载状态中（收发消息）多
%% 频繁的FULL GC会影响整个队列的效率，尤其是一个Erlang进程
%% 消耗了大量的内存，但是垃圾量非常少
%% GC的时间消耗的越久，越有可能推迟下次GC的时间
%% 如果GC的时间非常短，那么就会GC的越频繁

%% GC时间短的情况，
%% 1.进程非常少，垃圾也非常少（生产的快，消耗的也快）
%% 2.进程非常多，但是处在停顿状态的进程非常少（生产和消费者多，搞吞吐状态，生产消费都快）
%% 3.进程非常多，垃圾非常少 （生产和消费者多，生产消费都非常少）
%% GC时间长的情况
%% 1.进程非常少，垃圾非常多 （生产快，消费者慢）
%% 2.进程非常多，垃圾非常多 （生产快，消费的慢，生产者和消费者都非常多）
%% 3.进程多，处在休眠的非常多 （生产者消费者多，但是消息频率比较低）
-module(background_gc).

-behaviour(gen_server2).

-export([start_link/0, run/0]).
-export([gc/0]). %% For run_interval only

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_RATIO, 0.01).
-define(IDEAL_INTERVAL, 60000).

-record(state, {last_interval}).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-spec(start_link/0 :: () -> {'ok', pid()} | {'error', any()}).
-spec(run/0 :: () -> 'ok').
-spec(gc/0 :: () -> 'ok').

-endif.

%%----------------------------------------------------------------------------

start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [],
                                       [{timeout, infinity}]).

run() -> gen_server2:cast(?MODULE, run).

%%----------------------------------------------------------------------------

init([]) -> {ok, interval_gc(#state{last_interval = ?IDEAL_INTERVAL})}.

handle_call(Msg, _From, State) ->
    {stop, {unexpected_call, Msg}, {unexpected_call, Msg}, State}.

handle_cast(run, State) -> gc(), {noreply, State};

handle_cast(Msg, State) -> {stop, {unexpected_cast, Msg}, State}.

handle_info(run, State) -> {noreply, interval_gc(State)};

handle_info(Msg, State) -> {stop, {unexpected_info, Msg}, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) -> State.

%%----------------------------------------------------------------------------

interval_gc(State = #state{last_interval = LastInterval}) ->
    {ok, Interval} = rabbit_misc:interval_operation(
                       {?MODULE, gc, []},
                       ?MAX_RATIO, ?IDEAL_INTERVAL, LastInterval),
    erlang:send_after(Interval, self(), run),
    State#state{last_interval = Interval}.
%% 对处在暂停状态的进程进行FULL GC
gc() ->
    [garbage_collect(P) || P <- processes(),
                           {status, waiting} == process_info(P, status)],
    garbage_collect(), %% since we will never be waiting...
    ok.
