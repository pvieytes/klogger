klogger
=======

* [What is klogger?](#about)
* [Author](#author)
* [Usage](#usage)
* [License](#license)
* [TODO](#todo)


## What is klogger? <a name="about"></a>


**klogger** is a erlang logger with console and file backend.



## Author <a name="author"></a>

This is an [openshine](http://www.openshine.com) project developed by:
  * Pablo Vieytes


## Usage <a name="usage"></a>

start the application
```erlang
klogger:start().
```


No logger is started by default, so, at least one should be added to start to log.
If no options are given when is added, it will have console backend and the higher log level (everything will be showed).

```erlang
klogger:add_logger(logger).
logger:debug("text message").
```
The logger name ('logger' in this case) must be an atom. You should be careful with this name. 
Some code will be compiled dynamicaly and loaded as logger name, so this could override some existing module.

Options is a tuple or list of tuples.

For console backend:
```erlang
{ 
    console_backend, 
    BackendName::atom(),
    LogLevel:: integer() 
}
```


For file backend:
```erlang
{ 
    file_backend, 
    BackendName::atom(),
    FilePath::string(),
    LogLevel:: integer() 
}
```
There are defined some log level macros in include/klogger.hrl file.


Add new logger with options:

```erlang
Options = [{file_backend, file_log, "/tmp/test.log", ?WARNING},
           {console_backend, console_log, ?DEBUG}]
klogger:add_logger(logger, Options).
```



## License <a name="license"></a>

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. 
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

## TODO <a name="todo"></a>
* Erlang error logger integration

