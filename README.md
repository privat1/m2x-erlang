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

To get an interactive shell, you'll need to install the [Erlang/OTP](https://github.com/erlang/otp/wiki/Installation) virtual machine and the [Rebar](https://github.com/rebar/rebar) build system and dependency manager, either through your system's package manager or by builting from source.

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

You can then use the `Client` function to call API endpoints directly, as in the following example. The result is returned as a tuple containing the status as an atom (`ok`/`client_error`/`server error`), the status as an integer HTTP status code, and the response body decoded from JSON into a list.

```erlang
> Client({get, <<"/status">>}).
{ok,200,[{<<"api">>,<<"OK">>},{<<"triggers">>,<<"OK">>}]}
```

You can also add parameters to be sent in the request body as JSON encoded from an erlang list. See the documentation for [the jsx library](https://github.com/talentdeficit/jsx) for more information about the mapping between JSON types and erlang types.

```erlang
> Params = [{<<"foo">>,<<"Foo value">>},{<<"bar">>,<<"Bar value">>}],
> Client({put, <<"/no/such/endpoint">>, Params}).
{client_error,404,
              [{<<"message">>,
                <<"The specified resource does not exist">>}]}
```

To avoid using API endpoints directly, the `Client` function can instead be passed as the first argument to one of the many convenience wrappers provided by this library. The following are examples of some, but not all of the available wrapper functions.

- [m2x](src/m2x.erl)
```erlang
m2x:devices(Client).
m2x:create_device(Client, Params).
```

- [m2x_device](src/m2x_device.erl)
```erlang
m2x_device:view(Client, <<"DEVICE-ID">>).
m2x_device:update(Client, <<"DEVICE-ID">>, Params).
```

- [m2x_stream](src/m2x_stream.erl)
```erlang
m2x_stream:view(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
m2x_stream:update(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

- [m2x_trigger](src/m2x_trigger.erl)
```erlang
m2x_trigger:view(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>).
m2x_trigger:update(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>, Params).
```

- [m2x_distribution](src/m2x_distribution.erl)
```erlang
m2x_distribution:view(Client, <<"DISTRIBUTION-ID">>).
m2x_distribution:update(Client, <<"DISTRIBUTION-ID">>, Params).
```

- [m2x_distribution_stream](src/m2x_distribution_stream.erl)
```erlang
m2x_distribution_stream:view(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>).
m2x_distribution_stream:update(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>, Params).
```

- [m2x_distribution_trigger](src/m2x_distribution_trigger.erl)
```erlang
m2x_distribution_trigger:view(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>).
m2x_distribution_trigger:update(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>, Params).
```

## Versioning

This library aims to adhere to [Semantic Versioning 2.0.0](http://semver.org/). As a summary, given a version number `MAJOR.MINOR.PATCH`:

1. `MAJOR` will increment when backwards-incompatible changes are introduced to the client.
2. `MINOR` will increment when backwards-compatible functionality is added.
3. `PATCH` will increment with backwards-compatible bug fixes.

Additional labels for pre-release and build metadata are available as extensions to the `MAJOR.MINOR.PATCH` format.

**Note**: the client version does not necessarily reflect the version used in the AT&T M2X API.

## License

This library is provided under the MIT license. See [LICENSE](LICENSE) for applicable terms.
