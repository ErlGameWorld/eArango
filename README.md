# eArango

    arangodb erlang driver
    erlang otp21.2+ arangodb 3.6.2 3.7 3.9

# Feature

Efficient, fast and easy to use.

1. This driver can use connection pooling or simply establish multiple connections in a single process (non-connection
   pooling mode) for various data operations. Synchronous and asynchronous operations are supported when using
   connection pooling, and you need to save the requestId extra if you want to use asynchronous operations Waiting for
   the received data to return, the API encapsulated by the current driver all USES synchronous operation, and can be
   modified if asynchronous operation is needed. Only synchronous operations are supported for single-process
   operations. In single-process mode, compared with connection pooling mode, data replication between processes can be
   reduced once. For operation of large amount of data, database connection can be established separately in data
   management process instead of connection pooling.
2. The connection pooling mode and connectionless pool mode API interface ensures the identity, does not need to be
   treated differently, and is easy to understand and change between connection pooling mode and connectionless pool
   mode.

# Batch requests are not supported

    https://www.arangodb.com/docs/stable/http/batch-request.html 

# compile

    rebar3 compile

# how to use

    revar3: rebar3 shell
    Non-connection pooling mode
    Make a connection first
        {ok, Socket} = agVstCli:connect([]).           %% Use default Settings
        %% Then you can then call various apis using Socket as the first argument
        agMgrDb:curDbInfo(Socket).
    
    Connection pooling mode
       application:ensure_all_started(eArango).        %% start app
       agVstCli:startPool(poolName, [], []).            %% start pool
       %% Then you can then invoke various apis using poolName as the first argument
       agMgrDb:curDbInfo(poolName).  

# notice
   该驱动中所有的字符串 一律值 二进制的字符串
   QueryPars 和 Headers为map 其中的 key -> value 必须为字符串

       
