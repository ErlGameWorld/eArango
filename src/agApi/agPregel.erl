-module(agPregel).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/pregel.html

% 需要具有这些属性的 JSON 对象：
% algorithm：算法的名称。之一：
%    "pagerank" - 网页排名
%    "sssp" - 单源最短路径
%    "connectedcomponents" - 连接组件
%    "wcc" - 弱连接组件
%    "scc" - 强连接组件
%    "hits" - 超链接引发的主题搜索
%    "effectivecloseness" - 有效的亲密度
%    "linerank" - LineRank
%    "labelpropagation" - 标签传播
%    "slpa" - 说话者-听者标签传播
% graphName : 图的名称。this 或参数vertexCollectionsand edgeCollections都是必需的。请注意，为了与 Pregel 一起使用，图有特殊的分片要求。
% vertexCollections：顶点集合名称列表。请注意，为了与 Pregel 一起使用，集合有特殊的分片要求。
% edgeCollections：边集合名称列表。请注意，为了与 Pregel 一起使用，集合有特殊的分片要求。
% params：通用以及特定于算法的选项。
%
% 最重要的通用选项是“store”，它控制是否将 Pregel 作业计算的结果写回源集合。
% 另一个重要的通用选项是“parallelism”，它控制最多可用于 Pregel 作业的并行线程数。如果未指定“parallelism”，则可以使用默认值。此外，“并行性”的值可能有效地限制在某些特定于服务器的值上。
% “useMemoryMaps”选项控制是否使用基于磁盘的文件来存储临时结果。这可能会使计算受到磁盘限制，但允许您运行不适合主内存的计算。建议为较大的数据集设置此标志。
% 属性“shardKeyAttribute”指定边缘集合被分片的分片键（默认值：）"vertex"。
% 要开始执行，您需要指定算法名称和命名图（集群中的 SmartGraph）。或者，您可以指定顶点和边集合。此外，您可以指定因每种算法而异的自定义参数，请参阅Pregel - 可用算法。
% 返回代码
% 200：如果成功创建了 Pregel 并且回复正文是一个字符串，则返回 HTTP 200id以查询状态或取消执行。
% 400：如果 Pregel 作业的集合集合包含系统集合，或者集合不符合 Pregel 作业的分片要求，则返回 HTTP 400 错误。
% 403：如果没有足够的权限访问为 Pregel 作业指定的集合，则返回 HTTP 403 错误。
% 404：如果未找到指定的“算法”，或未找到“graphName”中指定的图形，或至少未找到“vertexCollections”或“edgeCollections”中指定的集合，则返回 HTTP 404 错误。
run(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/control_pregel">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% 获取 Pregel 作业执行状态
% GET /_api/control_pregel/{id}
% 查询参数
% id（必需）：Pregel 执行标识符。
% 返回执行的当前状态、当前全局超级步、运行时、全局聚合器值以及发送和接收消息的数量。
% HTTP 200如果作业执行 ID 有效并且状态与响应一起返回，则将返回 HTTP 200。
% state : 执行状态。可以返回以下值：
%    "running": 算法正常执行。
%    "storing": 算法完成，但结果仍在写回集合中。仅当 store 参数设置为 true 时发生。
%    "done": 执行完毕。在 3.7.1 及更高版本中，这意味着也完成了存储。在早期版本中，结果可能尚未写回到集合中。此事件在服务器日志中公布（至少需要pregel日志主题的info 日志级别）。
%    "canceled"：执行已被用户或错误永久取消。
%    "fatal error": 执行失败，无法恢复。
%    "in error"（当前未使用）：执行处于错误状态。这可能是由于无法访问 DB-Server 或无响应造成的。执行可能会稍后恢复，或者"canceled"如果无法成功恢复则切换到。
%    "recovering"(当前未使用)：执行正在积极恢复，running如果恢复成功将切换回。
% gss：执行的全局超级步数。
% totalRuntime：到目前为止执行的总运行时间（如果执行仍在进行中）。
% startupTime：执行的启动运行时。启动时间包括数据加载时间，并且可能很长。如果启动仍在进行中，则启动时间将报告为 0。
% 计算时间：算法执行时间。如果计算仍在进行中，计算时间将报告为 0。
% storageTime : 如果作业包含结果存储，则存储结果的时间。如果存储结果仍在进行中，则存储时间报告为 0。
% 报告：有关 Pregel 执行的统计信息。只有在算法完成后才会填充该值。
% vertexCount：处理的顶点总数。
% edgeCount：处理的边总数。
% 404：如果没有找到指定执行编号的Pregel作业或执行编号无效，则返回HTTP 404错误。

status(PoolNameOrSocket, PregelId) ->
   Path = <<"/_api/control_pregel/", (agMiscUtils:toBinary(PregelId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, Path).

% 获取当前正在运行的 Pregel 作业
% 获取当前正在运行的 Pregel 作业的概述
% GET /_api/control_pregel
% 返回当前正在运行和最近完成的 Pregel 作业的列表，而不检索它们的结果。返回的对象是 Pregel 职位描述的 JSON 数组。每个职位描述都是一个 JSON 对象，具有以下属性：
% id：Pregel 作业的 id，作为字符串。
% algorithm：作业使用的算法。
% created：创建作业的日期和时间。
% expires：作业结果过期的日期和时间。到期日期仅对已完成、取消或导致错误的作业有意义。当此类作业达到其到期日期/时间时，垃圾收集器会对其进行清理。
% ttl：作业结果的 TTL（生存时间）值，以秒为单位。TTL 用于计算作业结果的到期日期。
% state：执行的状态，作为一个字符串。
% gss：执行了许多全局超级步骤。
% totalRuntime：到目前为止执行的总运行时间（如果执行仍在进行中）。
% startupTime：执行的启动运行时。启动时间包括数据加载时间，并且可能很长。如果启动仍在进行中，则启动时间报告为 0。
% 计算时间：算法执行时间。如果计算仍在进行，则计算时间报告为 0。
% storageTime：如果作业包含结果存储，则存储结果的时间。如果存储结果仍在进行中，则存储时间报告为 0。
% 报告：有关 Pregel 执行的可选统计信息。该值仅在算法完成时填充。
% 回应
% HTTP 200 : 成功获取作业列表时返回。
getRunning(PoolNameOrSocket) ->
   Path = <<"/_api/control_pregel">>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, Path).

% 取消正在进行的 Pregel 执行
% DELETE /_api/control_pregel/{id}
% 查询参数
% id（必需）：Pregel 执行标识符。
% 取消仍在运行的执行，并丢弃任何中间结果。这将立即释放执行占用的所有内存，并使您丢失所有中间数据。
% 如果您请求存储结果，然后在它已处于其"storing"状态（或"done"3.7.1 之前的版本中的状态） 时取消执行，则可能会得到不一致的结果。数据一次以多线程方式写入所有集合分片。这意味着同时存在多个事务。当您取消执行作业时，可能已经提交了事务。因此，您可能会看到一些更新的文档，而其他文档没有上次执行的结果或过时的结果。
% 返回代码
% 200：如果作业执行 ID 有效，将返回 HTTP 200。
% 404：如果没有找到指定执行编号的Pregel作业或执行编号无效，则返回HTTP 404错误。

cancel(PoolNameOrSocket, PregelId) ->
   Path = <<"/_api/control_pregel/", (agMiscUtils:toBinary(PregelId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path).
