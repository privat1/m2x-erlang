# AT&T M2X Erlang Client

[AT&T M2X](http://m2x.att.com) is a cloud-based fully managed time-series data storage service for network connected machine-to-machine (M2M) devices and the Internet of Things (IoT).

The [AT&T M2X API](https://m2x.att.com/developer/documentation/overview) provides all the needed operations and methods to connect your devices to AT&T's M2X service. This client provides an easy to use interface for [Erlang](http://www.erlang.org).

Refer to the [Glossary of Terms](https://m2x.att.com/developer/documentation/glossary) to understand the nomenclature used throughout this documentation.

## Getting Started
1. Signup for an [M2X Account](https://m2x.att.com/signup).
2. Obtain your _Master Key_ from the Master Keys tab of your [Account Settings](https://m2x.att.com/account) screen.
2. Create your first [Device](https://m2x.att.com/devices) and copy its _Device ID_.
3. Review the [M2X API Documentation](https://m2x.att.com/developer/documentation/overview).

## Getting an Interactive M2X-Erlang Shell

To get an interactive shell, you'll need to install the [Erlang/OTP](https://github.com/erlang/otp/wiki/Installation) virtual machine and the [Rebar](https://github.com/rebar/rebar) build system and dependency manager, either through your system's package manager or by building from the source.

You'll also need to clone this repository using Git.
```bash
git clone https://github.com/attm2x/m2x-erlang.git
cd m2x-erlang
```

From the repository directory, use `rebar` to fetch dependencies and compile the library.
```bash
rebar get-deps && rebar compile
```

Once the library and its dependencies are built, you can use `rebar` to get an interactive shell and follow along with the usage examples.
```bash
rebar shell
```

## Usage

In order to communicate with the M2X API, you first need to create a client function containing your API key.

```erlang
> Client = m2x:client(<<"YOUR-API-KEY">>).
#Fun<m2x_client.0.118548121>
```

You can then use the `Client` function to call API endpoints directly, as in the following example. The client function accepts a single tuple argument, containing the HTTP method as an atom (`get`/`put`/`post`/`delete`) and the endpoint as a binary. The result is returned as a tuple containing the status as an atom (`ok`/`client_error`/`server_error`), the status as an integer HTTP status code, and the response body decoded from JSON into a list.

```erlang
> Client({get, <<"/status">>}).
{ok,200,[{<<"api">>,<<"OK">>},{<<"triggers">>,<<"OK">>}]}
```

You can also add parameters to be sent in the request body as JSON encoded from an erlang list by adding them as a third element of the tuple passed to the client function. See the documentation for [the jsx library](https://github.com/talentdeficit/jsx) for more information about the mapping between JSON types and erlang types.

```erlang
> Params = [{<<"foo">>,<<"Foo value">>},{<<"bar">>,<<"Bar value">>}],
> Client({put, <<"/no/such/endpoint">>, Params}).
{client_error,404,
              [{<<"message">>,
                <<"The specified resource does not exist">>}]}
```

To avoid using API endpoints directly, the `Client` function can instead be passed as the first argument to one of the many convenience wrappers provided by this library. Follow the links to the documentation for the following modules to see all of the available wrapper functions with links to the relevant API documentation.

- **m2x** - [Module documentation](doc/m2x.md) - [Module source](src/m2x.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/overview)
- **m2x_key** - [Module documentation](doc/m2x_key.md) - [Module source](src/m2x_key.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/keys)
- **m2x_device** - [Module documentation](doc/m2x_device.md) - [Module source](src/m2x_device.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/device)
- **m2x_stream** - [Module documentation](doc/m2x_stream.md) - [Module source](src/m2x_stream.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/device)
- **m2x_command** - [Module documentation](doc/m2x_command.md) - [Module source](src/m2x_command.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/commands)
- **m2x_device_command** - [Module documentation](doc/m2x_device_command.md) - [Module source](src/m2x_device_command.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/commands)
- **m2x_distribution** - [Module documentation](doc/m2x_distribution.md) - [Module source](src/m2x_distribution.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/distribution)
- **m2x_distribution_stream** - [Module documentation](doc/m2x_distribution_stream.md) - [Module source](src/m2x_distribution_stream.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/distribution)
- **m2x_collection** - [Module documentation](doc/m2x_collection.md) - [Module source](src/m2x_collection.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/collections)
- **m2x_job** - [Module documentation](doc/m2x_job.md) - [Module source](src/m2x_job.erl) - [Web API documentation](https://m2x.att.com/developer/documentation/v2/jobs)

## Versioning

This library aims to adhere to [Semantic Versioning 2.0.0](http://semver.org/). As a summary, given a version number `MAJOR.MINOR.PATCH`:

1. `MAJOR` will increment when backwards-incompatible changes are introduced to the client.
2. `MINOR` will increment when backwards-compatible functionality is added.
3. `PATCH` will increment with backwards-compatible bug fixes.

Additional labels for pre-release and build metadata are available as extensions to the `MAJOR.MINOR.PATCH` format.

**Note**: the client version does not necessarily reflect the version used in the AT&T M2X API.

## License

This library is provided under the MIT license. See [LICENSE](LICENSE) for applicable terms.
