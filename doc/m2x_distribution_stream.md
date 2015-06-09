# m2x_distribution_stream

### view
Get details of a specific data stream associated with an existing distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#View-Data-Stream](https://m2x.att.com/developer/documentation/v2/distribution#View-Data-Stream)

```erlang
m2x_distribution_stream:view(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>)
```

### create, update
Update or create a data stream associated with the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Create-Update-Data-Stream](https://m2x.att.com/developer/documentation/v2/distribution#Create-Update-Data-Stream)

```erlang
m2x_distribution_stream:create(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>)
m2x_distribution_stream:create(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>, Params)
m2x_distribution_stream:update(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>, Params)
```

### delete
Delete an existing data stream associated with a specific distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Delete-Data-Stream](https://m2x.att.com/developer/documentation/v2/distribution#Delete-Data-Stream)

```erlang
m2x_distribution_stream:delete(Client, <<"DISTRIBUTION-ID">>, <<"STREAM-NAME">>)
```
