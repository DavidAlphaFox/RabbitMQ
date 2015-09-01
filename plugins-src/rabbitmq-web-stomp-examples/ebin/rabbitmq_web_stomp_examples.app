{application,rabbitmq_web_stomp_examples,
             [{description,"Rabbit WEB-STOMP - examples"},
              {vsn,"%%VSN%%"},
              {modules,[rabbit_web_stomp_examples_app]},
              {registered,[]},
              {mod,{rabbit_web_stomp_examples_app,[]}},
              {env,[{listener,[{port,15670}]}]},
              {applications,[kernel,stdlib,rabbitmq_web_dispatch,
                             rabbitmq_web_stomp]}]}.
