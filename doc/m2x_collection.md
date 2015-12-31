# m2x_collection

### view
Get details of an existing collection.
> [https://m2x.att.com/developer/documentation/v2/collections#View-Collection-Details](https://m2x.att.com/developer/documentation/v2/collections#View-Collection-Details)

```erlang
m2x_collection:view(Client, <<"COLLECTION-ID">>).
```

### update
Update an existing collection's information.
> [https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Details](https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Details)

```erlang
m2x_collection:update(Client, <<"COLLECTION-ID">>, Params).
```

### delete
Delete an existing device collection.
> [https://m2x.att.com/developer/documentation/v2/collections#Delete-Collection](https://m2x.att.com/developer/documentation/v2/collections#Delete-Collection)

```erlang
m2x_collection:delete(Client, <<"COLLECTION-ID">>).
```

### metadata
Get the custom metadata of the specified collection.
> [https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata](https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata)

```erlang
m2x_collection:metadata(Client, <<"DISTRIBUTION-ID">>).
```

### update_metadata
Update the custom metadata of the specified collection.
> [https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata](https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata)

```erlang
m2x_collection:update_metadata(Client, <<"DISTRIBUTION-ID">>, Params).
```

### get_metadata_field
Get the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata-Field](https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata-Field)

```erlang
m2x_collection:get_metadata_field(Client, <<"DISTRIBUTION-ID">>, K).
```

### set_metadata_field
Update the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata-Field](https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata-Field)

```erlang
m2x_collection:set_metadata_field(Client, <<"DISTRIBUTION-ID">>, K, V).
```
