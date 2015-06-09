# m2x_stream

### view
Get details of a specific data stream associated with an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#View-Data-Stream](https://m2x.att.com/developer/documentation/v2/device#View-Data-Stream)

```erlang
m2x_stream:view(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
```

### create, update
Update or create a data stream associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Create-Update-Data-Stream](https://m2x.att.com/developer/documentation/v2/device#Create-Update-Data-Stream)

```erlang
m2x_stream:create(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
m2x_stream:create(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
m2x_stream:update(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### delete
Delete an existing data stream associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream](https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream)

```erlang
m2x_stream:delete(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
```

### update_value
Update the current stream value of the specified stream.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Data-Stream-Value](https://m2x.att.com/developer/documentation/v2/device#Update-Data-Stream-Value)

```erlang
m2x_stream:update_value(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### values
List values from an existing data stream associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Data-Stream-Values](https://m2x.att.com/developer/documentation/v2/device#List-Data-Stream-Values)

```erlang
m2x_stream:values(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
m2x_stream:values(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### post_values
Post timestamped values to an existing data stream associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#Post-Data-Stream-Values](https://m2x.att.com/developer/documentation/v2/device#Post-Data-Stream-Values)

```erlang
m2x_stream:post_values(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### delete_values
Delete values in a stream by a date range.
> [https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream-Values](https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream-Values)

```erlang
m2x_stream:delete_values(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### sampling
Sample values from an existing data stream associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling](https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling)

```erlang
m2x_stream:sampling(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```

### stats
Return count, min, max, average and standard deviation stats for the values on an existing data stream.
> [https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling](https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling)

```erlang
m2x_stream:stats(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>).
m2x_stream:stats(Client, <<"DEVICE-ID">>, <<"STREAM-NAME">>, Params).
```
