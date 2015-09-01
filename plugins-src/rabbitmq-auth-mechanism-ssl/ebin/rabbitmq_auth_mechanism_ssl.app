{application,rabbitmq_auth_mechanism_ssl,
             [{description,"RabbitMQ SSL authentication (SASL EXTERNAL)"},
              {vsn,"%%VSN%%"},
              {modules,[rabbit_auth_mechanism_ssl,
                        rabbit_auth_mechanism_ssl_app]},
              {registered,[]},
              {mod,{rabbit_auth_mechanism_ssl_app,[]}},
              {env,[{name_from,distinguished_name}]},
              {applications,[kernel,stdlib]}]}.
