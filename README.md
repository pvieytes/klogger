klogger
=======





Erlang logger logger

klogger:start().

klogger:add_logger(logger, [console_log_backend, {file_log_backend, "/tmp/klogger.log"}]). 


klogger:set_log_level(logger, [{console_log_backend, 5}, {file_log_backend, 3}]).

logger:log(debug, "msg txt").
logger:log(5, "msg txt").
logger:debug("msg txt").


logger:log(fatal, "msg txt ").

