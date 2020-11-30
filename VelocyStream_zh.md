Client / Server Communication (VST 1.1)
=======================================

Version 1.1.0 of 23 November 2016

# HTTP
    使用VelocyPack作为身体。内容类型为"application/vpack"

Binary Protocol
---------------
    这不是请求/响应协议。它是对称的（原则上）。消息可以来回发送，流水线，多路复用，单向或双向。
    可能会生成一条消息
    没有反应
    只是一个回应
    多重回应
    
    该VelocyStream并没有强加或指定或要求上述行为之一。
    应用程序必须在一般情况下或根据每个请求定义行为，
    请参见下文。然后，服务器和客户端必须实现该行为。

### Message vs. Chunk
    使用者（客户端或服务器）将处理消息。一条消息包含一个或多个VelocyPack（在某些情况下是二进制数据的某些部分）。
    消息中有多少VelocyPacks完全取决于应用程序，请参阅下文了解ArangoDB。

    消息可能很大。由于可以通过一个连接对消息进行多路复用，因此需要将大消息拆分为多个块。
    发送者/接收者类将接受一个VelocyPacks向量，将其分割成块，通过导线发送这些块，组装这些块，
    生成一个VelocyPacks向量并将其传递给消费者。

### Chunks
    为了允许重组块，每个程序包都以一个 header 作为前缀。块始终至少为24个字节长。字节顺序始终是小端。
    块的格式如下，无论它是消息中的第一个消息还是随后的消息：

    名称	            类型	                描述
    length	        uint32_t	        当前块（包括此标头）的总长度（以字节为单位）
    chunkX	        uint32_t	        chunk / isFirstChunk（高31位/最低位），详细信息请参见下文
    messageId	    uint64_t	        唯一标识符，发送方有责任生成这样的标识符（zero is reserved for not set ID）
    messageLength	uint64_t	        the total size of the message.
    Binary data	    binary data blob	size b1 |

    声明："chunk" and "isFirstChunk" are combined into an unsigned 32bit value.
    Therefore it will be encoded as
    uint32_t chunkX and extracted as

    chunk = chunkX >> 1
    isFirstChunk = chunkX & 0x1
    
    对于消息的第一块，设置第二个uint32_t的低位，对于所有后续消息，将其复位。
    在消息的第一个块中，数字 chunk 是消息中的块总数，在所有后续块中，数字 chunk 是该块的当前number。

    数据包的总大小为（24 + b1）字节。此数字存储在length字段中。
    如果需要发送大于UINT32_MAX的消息，则必须将这些消息分块。通常，最好将最大大小限制为几兆字节。

### **Notes:**
    发送（小）消息时，（出于性能原因）确保仅发送一个TCP数据包非常重要。
    例如，通过在Linux下使用sendmmsg
    ([https://blog.cloudflare.com/how-to-receive-a-million-packets](https://blog.cloudflare.com/how-to-receive-a-million-packets/))
    但是，实现者应该意识到，在TCP / IP中不能强制执行此操作，因此实现者必须始终意识到，
    网络堆栈的某些部分可以拆分数据包，并且有效负载可能分成多个部分！

ArangoDB
========
### Request / Response
    对于ArangoDB客户端，请求的格式如下，该数组是VelocyPack数组：
    [
        /* 0 - version: */     1,                    // [int]
        /* 1 - type: */        1,                    // [int] 1=Req, 2=Res,..
        /* 2 - database: */    "test",               // [string]
        /* 3 - requestType: */ 1,                    // [int] 0=Delete, ...
        /* 4 - request: */     "/_api/collection",   // [string\]
        /* 5 - parameter: */   { force: true },      // [[string]->[string]]   http的请求参数列表
        /* 6 - meta: */        { x-arangodb: true }  // [[string]->[string]]   http的header
    ]
    Body (binary data)
    
    如果数据库未设置（entry is `null`），则数据库为“ _system”。

#### type：
    1 = Request
    2 = Response (final response for this message id)
    3 = Response (but at least one more response will follow)
    1000 = Authentication
#### requestType：
    0 = DELETE
    1 = GET
    2 = POST
    3 = PUT
    4 = HEAD (not used in VPP)
    5 = PATCH
    6 = OPTIONS (not used in VPP)
    
### For example:
    HTTP请求
        http://localhost:8529/_db/test/_admin/echo?a=1&b=2&c[]=1&c[]=3
    With header:：
        X-ArangoDB-Async: true
    is equivalent to
        [
          1,               // version
          1,               // type
          "test",          // database
          1,               // requestType GET
          "/_admin/echo",  // request path
          {                // parameters
            a: 1,
            b: 2,
            c: [ 1, 3 ]
          },
          {                // meta
            x-arangodb-async: true
          }
        ]
        
    该请求是一条以VelocyPack开头的消息。这个VelocyPack始终包含标题字段，参数和请求路径。
    如果meta字段不包含内容类型，则采用默认值 "application/vpack"，并且正文将是一个或多个VelocyPack对象。

#### The response will be
    [
      1,                // 0 - version
      2 or 3,           // 1 - type
      400,              // 2 - responseCode
      { etag: "1234" }  // 3 - meta: [[str]->[str]]
    ]

#### Body (binary data)
    请求可以通过管道传输或混合。使用标头中的“ messageId”映射响应。发送者有责任生成合适的“ messageId”值。

    默认内容类型为"application/vpack"。

### Authentication
    A connection can be authenticated with the following message:
    [
      1,              // version
      1000,           // type
      "plain",        // encryption
      "admin",        // user
      "plaintext",    // password
    ]
    or
    [
      1,              // version
      1000,           // type
      "jwt",          // encryption
      "abcd..."       // token
    ]
    The response is
    { "error": false }
    
    if successful or
    { 
      "error": true,
      "errorMessage": "MESSAGE",
      "errorCode": CODE
    }
    
    如果不成功，则在这种情况下服务器将关闭连接。可以使用/_open/auth与HTTP版本相同的语义，使用未经身份验证的开放式路由，以与HTTP相同的方式获取JWT令牌。这样，可以通过JWT在单个会话中完成完整的身份验证。

### Content-Type and Accept
    通常，内容类型将是VPP，即主体是存储为VelocyPack的对象。
    有时有必要使用非结构化数据（例如文本，css或html）进行响应。主体将是一个仅包含二进制属性的VelocyPack对象，并将相应地设置content-type。

规则如下。

#### Http
    Request: Content-Type
        "application/json"`: the body contains the JSON string representation
        `"application/vpack"`: the body contains a velocy pack

    有一些处理程序允许JSON列表（由换行符分隔）。在这种情况下，我们还允许不使用任何分隔符的多个速度包。

    Request: Accept
        "application/json"：如果可能，在正文中发送JSON字符串表示形式
        "application/vpack"：如果可能的话，在体内发送速度包

    如果请求是"application/json"或"application/vpack"处理程序产生了其他请求（即"application/html"），那么将忽略接受。
    
    如果请求被请求"application/json"并且处理程序产生 "application/vpack"，则VPACK将转换为JSON。
    
    如果请求被请求"application/vpack"，并且处理程序生成“ application / json”，则JSON将转换为VPACK。
    
#### VPP
    与HTTP相似，不同之处在于：不支持“ Accept”标头，并且"application/json"始终将其转换为“ application / vpack”。这意味着主体包含一个或多个速度包。通常，它将包含一个-值得注意的例外是导入。
    
    如果处理程序产生了其他东西（即"application/html"），则主体将是一个二进制blob（而不是一个velocy-pack），并且将相应地设置content-type。
    
    连接后发送的第一个字节（“客户端”端-即使程序是双向的，也有服务器在监听端口，而客户端在连接端口）

    VST/1.1\r\n\r\n
(11 Bytes)