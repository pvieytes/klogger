klogger
=======

Erlang logger loggerklogger:start().


klogger:add_logger(logger, [console_log_backend, {file_log_backend, "/tmp/klogger.log"}]). 
klogger:set_log_level(logger, [{console_log_backend, 0}, {file_log_backend, 2}]).

klogger:log(logger, info, "msg info").