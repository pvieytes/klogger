klogger
=======

* [What is klogger?](#about)
* [Author](#author)
* [Usage](#usage)
* [License](#license)
* [TODO](#todo)


## What is klogger? <a name="about"></a>

**klogger** is a erlang app to create loggers with console and file backend. 
Klogger can create more than one logger with diferents backends and log levels.


## Author <a name="author"></a>

This is an [openshine](http://www.openshine.com) project developed by:
  * Pablo Vieytes


## Usage <a name="usage"></a>

start the application
```erlang
klogger:start().
```


Klogger is multi logger application, but no logger is started by default, so, at least one should be added to start to log. You can add as many as you want to.
If no options are given when a logger is added, it will have console backend and the higher log level (every message will be showed).

```erlang
klogger:add_logger(logger).
logger:debug("text message").
```
The logger name ('logger' in this case) must be an atom. You should be careful with this name. 
Some code will be compiled dynamically and loaded as logger name.

Options could be a backend tuple or a list of backend tuples
Options for console backend:


```erlang
{backend, [backend_option()]}

backend_option() = name() | type() | loglevel() | level() | path() | get_error_logger()
name() = {name, Name::atom()}
type() = {type, file_backend | console_backend}
loglevel() = {loglevel, integer() | debug | info | warning | error | fatal | none}
path() = {path, Path::string()}
get_error_logger() =  {get_error_logger, enable | disable}
```
type() and name() are mandatory, and path() is also mandatory for file_backend loggers.

There are defined some integer macros:
  * ```?DEBUG```
  * ```?INFO```
  * ```?WARNING```
  * ```?ERROR```
  * ```?FATAL```
  * ```?NONE``` 




Add new logger with options:

```erlang
Options = [
 	{backend, [{name, console_log}, 
			   {type, console_backend},
			   {loglevel, debug},
			   {get_error_logger, enable}
			  ]},
		{backend,  [{name, file_log}, 
			    {type, file_backend},
			    {loglevel, debug},
			    {path, LogFilePath},
			    {get_error_logger, enable}
			   ]}
	       ]
klogger:add_logger(logger, Options).
```

Log level can be change at runtime.

```erlang
klogger:set_log_level(logger, {file_log, ?DEBUG}).
```

## License <a name="license"></a>

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. 
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

## TODO <a name="todo"></a>
* Erlang error logger integration

