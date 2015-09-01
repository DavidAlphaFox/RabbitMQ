-module(rabbit_amqp1_0_framing0).
-export([record_for/1, fields/1, encode/1, symbol_for/1, number_for/1]).
-include("rabbit_amqp1_0.hrl").
record_for({symbol, <<"amqp:header:list">>}) ->
    #'v1_0.header'{};
record_for({_, 112}) ->
    #'v1_0.header'{};
% 0x00000000:0x00000070

record_for({symbol, <<"amqp:delivery-annotations:map">>}) ->
    #'v1_0.delivery_annotations'{};
record_for({_, 113}) ->
    #'v1_0.delivery_annotations'{};
% 0x00000000:0x00000071

record_for({symbol, <<"amqp:message-annotations:map">>}) ->
    #'v1_0.message_annotations'{};
record_for({_, 114}) ->
    #'v1_0.message_annotations'{};
% 0x00000000:0x00000072

record_for({symbol, <<"amqp:properties:list">>}) ->
    #'v1_0.properties'{};
record_for({_, 115}) ->
    #'v1_0.properties'{};
% 0x00000000:0x00000073

record_for({symbol, <<"amqp:application-properties:map">>}) ->
    #'v1_0.application_properties'{};
record_for({_, 116}) ->
    #'v1_0.application_properties'{};
% 0x00000000:0x00000074

record_for({symbol, <<"amqp:data:binary">>}) ->
    #'v1_0.data'{};
record_for({_, 117}) ->
    #'v1_0.data'{};
% 0x00000000:0x00000075

record_for({symbol, <<"amqp:amqp-sequence:list">>}) ->
    #'v1_0.amqp_sequence'{};
record_for({_, 118}) ->
    #'v1_0.amqp_sequence'{};
% 0x00000000:0x00000076

record_for({symbol, <<"amqp:amqp-value:*">>}) ->
    #'v1_0.amqp_value'{};
record_for({_, 119}) ->
    #'v1_0.amqp_value'{};
% 0x00000000:0x00000077

record_for({symbol, <<"amqp:footer:map">>}) ->
    #'v1_0.footer'{};
record_for({_, 120}) ->
    #'v1_0.footer'{};
% 0x00000000:0x00000078

record_for({symbol, <<"amqp:received:list">>}) ->
    #'v1_0.received'{};
record_for({_, 35}) ->
    #'v1_0.received'{};
% 0x00000000:0x00000023

record_for({symbol, <<"amqp:accepted:list">>}) ->
    #'v1_0.accepted'{};
record_for({_, 36}) ->
    #'v1_0.accepted'{};
% 0x00000000:0x00000024

record_for({symbol, <<"amqp:rejected:list">>}) ->
    #'v1_0.rejected'{};
record_for({_, 37}) ->
    #'v1_0.rejected'{};
% 0x00000000:0x00000025

record_for({symbol, <<"amqp:released:list">>}) ->
    #'v1_0.released'{};
record_for({_, 38}) ->
    #'v1_0.released'{};
% 0x00000000:0x00000026

record_for({symbol, <<"amqp:modified:list">>}) ->
    #'v1_0.modified'{};
record_for({_, 39}) ->
    #'v1_0.modified'{};
% 0x00000000:0x00000027

record_for({symbol, <<"amqp:source:list">>}) ->
    #'v1_0.source'{};
record_for({_, 40}) ->
    #'v1_0.source'{};
% 0x00000000:0x00000028

record_for({symbol, <<"amqp:target:list">>}) ->
    #'v1_0.target'{};
record_for({_, 41}) ->
    #'v1_0.target'{};
% 0x00000000:0x00000029

record_for({symbol, <<"amqp:delete-on-close:list">>}) ->
    #'v1_0.delete_on_close'{};
record_for({_, 43}) ->
    #'v1_0.delete_on_close'{};
% 0x00000000:0x0000002b

record_for({symbol, <<"amqp:delete-on-no-links:list">>}) ->
    #'v1_0.delete_on_no_links'{};
record_for({_, 44}) ->
    #'v1_0.delete_on_no_links'{};
% 0x00000000:0x0000002c

record_for({symbol, <<"amqp:delete-on-no-messages:list">>}) ->
    #'v1_0.delete_on_no_messages'{};
record_for({_, 45}) ->
    #'v1_0.delete_on_no_messages'{};
% 0x00000000:0x0000002d

record_for({symbol, <<"amqp:delete-on-no-links-or-messages:list">>}) ->
    #'v1_0.delete_on_no_links_or_messages'{};
record_for({_, 46}) ->
    #'v1_0.delete_on_no_links_or_messages'{};
% 0x00000000:0x0000002e

record_for({symbol, <<"amqp:sasl-mechanisms:list">>}) ->
    #'v1_0.sasl_mechanisms'{};
record_for({_, 64}) ->
    #'v1_0.sasl_mechanisms'{};
% 0x00000000:0x00000040

record_for({symbol, <<"amqp:sasl-init:list">>}) ->
    #'v1_0.sasl_init'{};
record_for({_, 65}) ->
    #'v1_0.sasl_init'{};
% 0x00000000:0x00000041

record_for({symbol, <<"amqp:sasl-challenge:list">>}) ->
    #'v1_0.sasl_challenge'{};
record_for({_, 66}) ->
    #'v1_0.sasl_challenge'{};
% 0x00000000:0x00000042

record_for({symbol, <<"amqp:sasl-response:list">>}) ->
    #'v1_0.sasl_response'{};
record_for({_, 67}) ->
    #'v1_0.sasl_response'{};
% 0x00000000:0x00000043

record_for({symbol, <<"amqp:sasl-outcome:list">>}) ->
    #'v1_0.sasl_outcome'{};
record_for({_, 68}) ->
    #'v1_0.sasl_outcome'{};
% 0x00000000:0x00000044

record_for({symbol, <<"amqp:open:list">>}) ->
    #'v1_0.open'{};
record_for({_, 16}) ->
    #'v1_0.open'{};
% 0x00000000:0x00000010

record_for({symbol, <<"amqp:begin:list">>}) ->
    #'v1_0.begin'{};
record_for({_, 17}) ->
    #'v1_0.begin'{};
% 0x00000000:0x00000011

record_for({symbol, <<"amqp:attach:list">>}) ->
    #'v1_0.attach'{};
record_for({_, 18}) ->
    #'v1_0.attach'{};
% 0x00000000:0x00000012

record_for({symbol, <<"amqp:flow:list">>}) ->
    #'v1_0.flow'{};
record_for({_, 19}) ->
    #'v1_0.flow'{};
% 0x00000000:0x00000013

record_for({symbol, <<"amqp:transfer:list">>}) ->
    #'v1_0.transfer'{};
record_for({_, 20}) ->
    #'v1_0.transfer'{};
% 0x00000000:0x00000014

record_for({symbol, <<"amqp:disposition:list">>}) ->
    #'v1_0.disposition'{};
record_for({_, 21}) ->
    #'v1_0.disposition'{};
% 0x00000000:0x00000015

record_for({symbol, <<"amqp:detach:list">>}) ->
    #'v1_0.detach'{};
record_for({_, 22}) ->
    #'v1_0.detach'{};
% 0x00000000:0x00000016

record_for({symbol, <<"amqp:end:list">>}) ->
    #'v1_0.end'{};
record_for({_, 23}) ->
    #'v1_0.end'{};
% 0x00000000:0x00000017

record_for({symbol, <<"amqp:close:list">>}) ->
    #'v1_0.close'{};
record_for({_, 24}) ->
    #'v1_0.close'{};
% 0x00000000:0x00000018

record_for({symbol, <<"amqp:error:list">>}) ->
    #'v1_0.error'{};
record_for({_, 29}) ->
    #'v1_0.error'{};
% 0x00000000:0x0000001d

record_for({symbol, <<"amqp:coordinator:list">>}) ->
    #'v1_0.coordinator'{};
record_for({_, 48}) ->
    #'v1_0.coordinator'{};
% 0x00000000:0x00000030

record_for({symbol, <<"amqp:declare:list">>}) ->
    #'v1_0.declare'{};
record_for({_, 49}) ->
    #'v1_0.declare'{};
% 0x00000000:0x00000031

record_for({symbol, <<"amqp:discharge:list">>}) ->
    #'v1_0.discharge'{};
record_for({_, 50}) ->
    #'v1_0.discharge'{};
% 0x00000000:0x00000032

record_for({symbol, <<"amqp:declared:list">>}) ->
    #'v1_0.declared'{};
record_for({_, 51}) ->
    #'v1_0.declared'{};
% 0x00000000:0x00000033

record_for({symbol, <<"amqp:transactional-state:list">>}) ->
    #'v1_0.transactional_state'{};
record_for({_, 52}) ->
    #'v1_0.transactional_state'{};
% 0x00000000:0x00000034

record_for(Other) -> exit({unknown, Other}).


fields(#'v1_0.header'{}) -> record_info(fields, 'v1_0.header');
fields(#'v1_0.delivery_annotations'{}) -> record_info(fields, 'v1_0.delivery_annotations');
fields(#'v1_0.message_annotations'{}) -> record_info(fields, 'v1_0.message_annotations');
fields(#'v1_0.properties'{}) -> record_info(fields, 'v1_0.properties');
fields(#'v1_0.application_properties'{}) -> record_info(fields, 'v1_0.application_properties');
fields(#'v1_0.data'{}) -> record_info(fields, 'v1_0.data');
fields(#'v1_0.amqp_sequence'{}) -> record_info(fields, 'v1_0.amqp_sequence');
fields(#'v1_0.amqp_value'{}) -> record_info(fields, 'v1_0.amqp_value');
fields(#'v1_0.footer'{}) -> record_info(fields, 'v1_0.footer');
fields(#'v1_0.received'{}) -> record_info(fields, 'v1_0.received');
fields(#'v1_0.accepted'{}) -> record_info(fields, 'v1_0.accepted');
fields(#'v1_0.rejected'{}) -> record_info(fields, 'v1_0.rejected');
fields(#'v1_0.released'{}) -> record_info(fields, 'v1_0.released');
fields(#'v1_0.modified'{}) -> record_info(fields, 'v1_0.modified');
fields(#'v1_0.source'{}) -> record_info(fields, 'v1_0.source');
fields(#'v1_0.target'{}) -> record_info(fields, 'v1_0.target');
fields(#'v1_0.delete_on_close'{}) -> record_info(fields, 'v1_0.delete_on_close');
fields(#'v1_0.delete_on_no_links'{}) -> record_info(fields, 'v1_0.delete_on_no_links');
fields(#'v1_0.delete_on_no_messages'{}) -> record_info(fields, 'v1_0.delete_on_no_messages');
fields(#'v1_0.delete_on_no_links_or_messages'{}) -> record_info(fields, 'v1_0.delete_on_no_links_or_messages');
fields(#'v1_0.sasl_mechanisms'{}) -> record_info(fields, 'v1_0.sasl_mechanisms');
fields(#'v1_0.sasl_init'{}) -> record_info(fields, 'v1_0.sasl_init');
fields(#'v1_0.sasl_challenge'{}) -> record_info(fields, 'v1_0.sasl_challenge');
fields(#'v1_0.sasl_response'{}) -> record_info(fields, 'v1_0.sasl_response');
fields(#'v1_0.sasl_outcome'{}) -> record_info(fields, 'v1_0.sasl_outcome');
fields(#'v1_0.open'{}) -> record_info(fields, 'v1_0.open');
fields(#'v1_0.begin'{}) -> record_info(fields, 'v1_0.begin');
fields(#'v1_0.attach'{}) -> record_info(fields, 'v1_0.attach');
fields(#'v1_0.flow'{}) -> record_info(fields, 'v1_0.flow');
fields(#'v1_0.transfer'{}) -> record_info(fields, 'v1_0.transfer');
fields(#'v1_0.disposition'{}) -> record_info(fields, 'v1_0.disposition');
fields(#'v1_0.detach'{}) -> record_info(fields, 'v1_0.detach');
fields(#'v1_0.end'{}) -> record_info(fields, 'v1_0.end');
fields(#'v1_0.close'{}) -> record_info(fields, 'v1_0.close');
fields(#'v1_0.error'{}) -> record_info(fields, 'v1_0.error');
fields(#'v1_0.coordinator'{}) -> record_info(fields, 'v1_0.coordinator');
fields(#'v1_0.declare'{}) -> record_info(fields, 'v1_0.declare');
fields(#'v1_0.discharge'{}) -> record_info(fields, 'v1_0.discharge');
fields(#'v1_0.declared'{}) -> record_info(fields, 'v1_0.declared');
fields(#'v1_0.transactional_state'{}) -> record_info(fields, 'v1_0.transactional_state');
fields(_Other) -> unknown.


encode(Frame = #'v1_0.header'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 112, Frame);
encode(Frame = #'v1_0.delivery_annotations'{}) ->
    rabbit_amqp1_0_framing:encode_described('annotations', 113, Frame);
encode(Frame = #'v1_0.message_annotations'{}) ->
    rabbit_amqp1_0_framing:encode_described('annotations', 114, Frame);
encode(Frame = #'v1_0.properties'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 115, Frame);
encode(Frame = #'v1_0.application_properties'{}) ->
    rabbit_amqp1_0_framing:encode_described('map', 116, Frame);
encode(Frame = #'v1_0.data'{}) ->
    rabbit_amqp1_0_framing:encode_described('binary', 117, Frame);
encode(Frame = #'v1_0.amqp_sequence'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 118, Frame);
encode(Frame = #'v1_0.amqp_value'{}) ->
    rabbit_amqp1_0_framing:encode_described('*', 119, Frame);
encode(Frame = #'v1_0.footer'{}) ->
    rabbit_amqp1_0_framing:encode_described('annotations', 120, Frame);
encode(Frame = #'v1_0.received'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 35, Frame);
encode(Frame = #'v1_0.accepted'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 36, Frame);
encode(Frame = #'v1_0.rejected'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 37, Frame);
encode(Frame = #'v1_0.released'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 38, Frame);
encode(Frame = #'v1_0.modified'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 39, Frame);
encode(Frame = #'v1_0.source'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 40, Frame);
encode(Frame = #'v1_0.target'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 41, Frame);
encode(Frame = #'v1_0.delete_on_close'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 43, Frame);
encode(Frame = #'v1_0.delete_on_no_links'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 44, Frame);
encode(Frame = #'v1_0.delete_on_no_messages'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 45, Frame);
encode(Frame = #'v1_0.delete_on_no_links_or_messages'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 46, Frame);
encode(Frame = #'v1_0.sasl_mechanisms'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 64, Frame);
encode(Frame = #'v1_0.sasl_init'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 65, Frame);
encode(Frame = #'v1_0.sasl_challenge'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 66, Frame);
encode(Frame = #'v1_0.sasl_response'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 67, Frame);
encode(Frame = #'v1_0.sasl_outcome'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 68, Frame);
encode(Frame = #'v1_0.open'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 16, Frame);
encode(Frame = #'v1_0.begin'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 17, Frame);
encode(Frame = #'v1_0.attach'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 18, Frame);
encode(Frame = #'v1_0.flow'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 19, Frame);
encode(Frame = #'v1_0.transfer'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 20, Frame);
encode(Frame = #'v1_0.disposition'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 21, Frame);
encode(Frame = #'v1_0.detach'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 22, Frame);
encode(Frame = #'v1_0.end'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 23, Frame);
encode(Frame = #'v1_0.close'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 24, Frame);
encode(Frame = #'v1_0.error'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 29, Frame);
encode(Frame = #'v1_0.coordinator'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 48, Frame);
encode(Frame = #'v1_0.declare'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 49, Frame);
encode(Frame = #'v1_0.discharge'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 50, Frame);
encode(Frame = #'v1_0.declared'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 51, Frame);
encode(Frame = #'v1_0.transactional_state'{}) ->
    rabbit_amqp1_0_framing:encode_described('list', 52, Frame);
encode(undefined) -> null;
encode(Other) -> Other.


symbol_for(#'v1_0.header'{}) ->
    {symbol, <<"amqp:header:list">>};
symbol_for(#'v1_0.delivery_annotations'{}) ->
    {symbol, <<"amqp:delivery-annotations:map">>};
symbol_for(#'v1_0.message_annotations'{}) ->
    {symbol, <<"amqp:message-annotations:map">>};
symbol_for(#'v1_0.properties'{}) ->
    {symbol, <<"amqp:properties:list">>};
symbol_for(#'v1_0.application_properties'{}) ->
    {symbol, <<"amqp:application-properties:map">>};
symbol_for(#'v1_0.data'{}) ->
    {symbol, <<"amqp:data:binary">>};
symbol_for(#'v1_0.amqp_sequence'{}) ->
    {symbol, <<"amqp:amqp-sequence:list">>};
symbol_for(#'v1_0.amqp_value'{}) ->
    {symbol, <<"amqp:amqp-value:*">>};
symbol_for(#'v1_0.footer'{}) ->
    {symbol, <<"amqp:footer:map">>};
symbol_for(#'v1_0.received'{}) ->
    {symbol, <<"amqp:received:list">>};
symbol_for(#'v1_0.accepted'{}) ->
    {symbol, <<"amqp:accepted:list">>};
symbol_for(#'v1_0.rejected'{}) ->
    {symbol, <<"amqp:rejected:list">>};
symbol_for(#'v1_0.released'{}) ->
    {symbol, <<"amqp:released:list">>};
symbol_for(#'v1_0.modified'{}) ->
    {symbol, <<"amqp:modified:list">>};
symbol_for(#'v1_0.source'{}) ->
    {symbol, <<"amqp:source:list">>};
symbol_for(#'v1_0.target'{}) ->
    {symbol, <<"amqp:target:list">>};
symbol_for(#'v1_0.delete_on_close'{}) ->
    {symbol, <<"amqp:delete-on-close:list">>};
symbol_for(#'v1_0.delete_on_no_links'{}) ->
    {symbol, <<"amqp:delete-on-no-links:list">>};
symbol_for(#'v1_0.delete_on_no_messages'{}) ->
    {symbol, <<"amqp:delete-on-no-messages:list">>};
symbol_for(#'v1_0.delete_on_no_links_or_messages'{}) ->
    {symbol, <<"amqp:delete-on-no-links-or-messages:list">>};
symbol_for(#'v1_0.sasl_mechanisms'{}) ->
    {symbol, <<"amqp:sasl-mechanisms:list">>};
symbol_for(#'v1_0.sasl_init'{}) ->
    {symbol, <<"amqp:sasl-init:list">>};
symbol_for(#'v1_0.sasl_challenge'{}) ->
    {symbol, <<"amqp:sasl-challenge:list">>};
symbol_for(#'v1_0.sasl_response'{}) ->
    {symbol, <<"amqp:sasl-response:list">>};
symbol_for(#'v1_0.sasl_outcome'{}) ->
    {symbol, <<"amqp:sasl-outcome:list">>};
symbol_for(#'v1_0.open'{}) ->
    {symbol, <<"amqp:open:list">>};
symbol_for(#'v1_0.begin'{}) ->
    {symbol, <<"amqp:begin:list">>};
symbol_for(#'v1_0.attach'{}) ->
    {symbol, <<"amqp:attach:list">>};
symbol_for(#'v1_0.flow'{}) ->
    {symbol, <<"amqp:flow:list">>};
symbol_for(#'v1_0.transfer'{}) ->
    {symbol, <<"amqp:transfer:list">>};
symbol_for(#'v1_0.disposition'{}) ->
    {symbol, <<"amqp:disposition:list">>};
symbol_for(#'v1_0.detach'{}) ->
    {symbol, <<"amqp:detach:list">>};
symbol_for(#'v1_0.end'{}) ->
    {symbol, <<"amqp:end:list">>};
symbol_for(#'v1_0.close'{}) ->
    {symbol, <<"amqp:close:list">>};
symbol_for(#'v1_0.error'{}) ->
    {symbol, <<"amqp:error:list">>};
symbol_for(#'v1_0.coordinator'{}) ->
    {symbol, <<"amqp:coordinator:list">>};
symbol_for(#'v1_0.declare'{}) ->
    {symbol, <<"amqp:declare:list">>};
symbol_for(#'v1_0.discharge'{}) ->
    {symbol, <<"amqp:discharge:list">>};
symbol_for(#'v1_0.declared'{}) ->
    {symbol, <<"amqp:declared:list">>};
symbol_for(#'v1_0.transactional_state'{}) ->
    {symbol, <<"amqp:transactional-state:list">>};
symbol_for(Other) -> exit({unknown, Other}).


number_for(#'v1_0.header'{}) ->
    {ulong, 112};
number_for(#'v1_0.delivery_annotations'{}) ->
    {ulong, 113};
number_for(#'v1_0.message_annotations'{}) ->
    {ulong, 114};
number_for(#'v1_0.properties'{}) ->
    {ulong, 115};
number_for(#'v1_0.application_properties'{}) ->
    {ulong, 116};
number_for(#'v1_0.data'{}) ->
    {ulong, 117};
number_for(#'v1_0.amqp_sequence'{}) ->
    {ulong, 118};
number_for(#'v1_0.amqp_value'{}) ->
    {ulong, 119};
number_for(#'v1_0.footer'{}) ->
    {ulong, 120};
number_for(#'v1_0.received'{}) ->
    {ulong, 35};
number_for(#'v1_0.accepted'{}) ->
    {ulong, 36};
number_for(#'v1_0.rejected'{}) ->
    {ulong, 37};
number_for(#'v1_0.released'{}) ->
    {ulong, 38};
number_for(#'v1_0.modified'{}) ->
    {ulong, 39};
number_for(#'v1_0.source'{}) ->
    {ulong, 40};
number_for(#'v1_0.target'{}) ->
    {ulong, 41};
number_for(#'v1_0.delete_on_close'{}) ->
    {ulong, 43};
number_for(#'v1_0.delete_on_no_links'{}) ->
    {ulong, 44};
number_for(#'v1_0.delete_on_no_messages'{}) ->
    {ulong, 45};
number_for(#'v1_0.delete_on_no_links_or_messages'{}) ->
    {ulong, 46};
number_for(#'v1_0.sasl_mechanisms'{}) ->
    {ulong, 64};
number_for(#'v1_0.sasl_init'{}) ->
    {ulong, 65};
number_for(#'v1_0.sasl_challenge'{}) ->
    {ulong, 66};
number_for(#'v1_0.sasl_response'{}) ->
    {ulong, 67};
number_for(#'v1_0.sasl_outcome'{}) ->
    {ulong, 68};
number_for(#'v1_0.open'{}) ->
    {ulong, 16};
number_for(#'v1_0.begin'{}) ->
    {ulong, 17};
number_for(#'v1_0.attach'{}) ->
    {ulong, 18};
number_for(#'v1_0.flow'{}) ->
    {ulong, 19};
number_for(#'v1_0.transfer'{}) ->
    {ulong, 20};
number_for(#'v1_0.disposition'{}) ->
    {ulong, 21};
number_for(#'v1_0.detach'{}) ->
    {ulong, 22};
number_for(#'v1_0.end'{}) ->
    {ulong, 23};
number_for(#'v1_0.close'{}) ->
    {ulong, 24};
number_for(#'v1_0.error'{}) ->
    {ulong, 29};
number_for(#'v1_0.coordinator'{}) ->
    {ulong, 48};
number_for(#'v1_0.declare'{}) ->
    {ulong, 49};
number_for(#'v1_0.discharge'{}) ->
    {ulong, 50};
number_for(#'v1_0.declared'{}) ->
    {ulong, 51};
number_for(#'v1_0.transactional_state'{}) ->
    {ulong, 52};
number_for(Other) -> exit({unknown, Other}).
