namespace ClmSys

open System
open System.ServiceModel
open ClmSys.GeneralData
open ClmSys.GeneralErrors

/// See https://stackoverflow.com/questions/53536450/merging-discriminated-unions-in-f
module Wcf =

    let connectionTimeOut = TimeSpan(0, 10, 0)
    let dataTimeOut = TimeSpan(1, 0, 0)


    let getBinding() =
        let binding = new NetTcpBinding()
        binding.MaxReceivedMessageSize <- (int64 Int32.MaxValue)
        binding.MaxBufferSize <- Int32.MaxValue
        binding.OpenTimeout <- connectionTimeOut
        binding.CloseTimeout <- connectionTimeOut
        binding.SendTimeout <- dataTimeOut
        binding.ReceiveTimeout <- dataTimeOut
        binding


    let tryGetWcfService<'T> url =
        try
            let binding = getBinding()
            let address = new EndpointAddress(url)
            let channelFactory = new ChannelFactory<'T>(binding, address)
            let service = channelFactory.CreateChannel()
            Ok (service, fun () -> channelFactory.Close())
        with
        | e -> e |> WcfException |> Error


    let toWcfError f e = e |> WcfException |> f |> Error
    let toWcfSerializationError f e = e |> WcfSerializationError |> f |> Error


    /// Client communication with the server.
    /// Note that this is a generic with 4 implicit parameters.
    /// We can bake in the first one into t at the caller.
    /// However, to do that here required assigning and using all 4.
    let tryCommunicate t c f a =
        try
            match t() with
            | Ok (service, factoryCloser) ->
                try
                    match a |> trySerialize with
                    | Ok b ->
                        c service b
                        |> tryDeserialize
                        |> Result.mapError WcfSerializationError
                        |> Result.mapError f
                        |> Result.bind id
                    | Error e -> toWcfSerializationError f e
                finally
                    let channel = (box service) :?> IClientChannel
                    channel.Close()
                    factoryCloser()
            | Error e -> e |> f |> Error
        with
        | e -> toWcfError f e


    /// Server reply.
    let tryReply p f a =
        let reply =
            match a |> tryDeserialize with
            | Ok m -> p m
            | Error e -> toWcfSerializationError f e

        match reply |> trySerialize with
        | Ok r -> r
        | Error _ -> [||]
