-module(agAdminMonitor).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/administration-and-monitoring.html

% 从服务器读取全局日志
%
% GET /_admin/log/entries
% 查询参数
%    upto（可选）：返回所有日志条目直到日志级别upto。请注意，最多必须是：
%       致命或0
%       错误或1
%       警告或2
%       信息或3
% debug 或4 默认值为info。
%       level（可选）：返回日志级别level 的所有日志条目。注意查询参数 upto和level是互斥的。
%       start（可选）：返回所有日志条目，使其日志条目标识符（lid值）大于或等于start。
%       size（可选）：将结果限制为最多大小日志条目。
%       offset（可选）：开始返回跳过第一个偏移日志条目的日志条目。偏移量 和大小可用于分页。
%       search（可选）：仅返回包含search 中指定文本的日志条目。
%       sort（可选）：根据日志条目的id值对日志条目进行升序（如果sort是asc）或降序（如果sort是desc）排序。请注意，id 强加了时间顺序。默认值为asc。
%       serverId（可选）：返回指定服务器的所有日志条目。所有其他查询参数保持有效。如果没有给出 serverId，被询问的服务器将回复。这个参数只对 Coordinator 有意义。
% 从服务器的全局日志返回致命、错误、警告或信息日志消息。结果是具有以下属性的 JSON 对象：
%       total : 分页前的日志条目总数
%       messages : 一个包含符合条件的日志消息的数组
% 这个 API 可以通过启动选项关闭--log.api-enabled。如果 API 被禁用，所有请求都将使用 HTTP 403 响应。如果 API 被启用，访问它需要管理员权限，甚至超级用户权限，具体取决于--log.api-enabled启动选项的值。
% 返回代码
%    200：如果请求有效则返回。
%    400：如果为upto或level指定了无效值，则返回。
%    403：访问日志权限不足返回。
getAdminLog(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/log/entries">>).

getAdminLog(PoolNameOrSocket, QueryPars) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/log/entries">>, QueryPars).

% 返回当前的日志级别设置
% GET /_admin/log/level
% 返回服务器的当前日志级别设置。结果是一个JSON对象，其日志主题为对象键，日志级别为对象值。
% 返回码
% 200：如果请求有效，则返回
% 500：如果服务器由于内存不足错误而无法生成结果，则返回。
getAdminLogLevel(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/log/level">>).

% 修改当前的日志级别设置
% PUT /_admin/log/level
% 具有以下属性的JSON对象是必需的：
% agency：可能的日志级别之一。
% agencycomm：可能的日志级别之一。
% authentication：可能的日志级别之一。
% 授权：可能的日志级别之一。
% cache：可能的日志级别之一。
% cluster：可能的日志级别之一。
% collector：可能的日志级别之一。
% 通信：可能的日志级别之一。
% 压缩器：可能的日志级别之一。
% config：可能的日志级别之一。
% datafiles：可能的日志级别之一。
% 开发：可能的日志级别之一。
% 引擎：可能的日志级别之一。
% general：可能的日志级别之一。
% graphs：可能的日志级别之一。
% 心跳：可能的日志级别之一。
% 内存：可能的日志级别之一。
% mmap：可能的日志级别之一。
% 性能：可能的日志级别之一。
% pregel：可能的对数水平之一。
% 查询：可能的日志级别之一。
% 复制：可能的日志级别之一。
% 请求：可能的日志级别之一。
% rocksdb：可能的日志级别之一。
% ssl：可能的日志级别之一。
% startup：可能的日志级别之一。
% 监视：可能的日志级别之一。
% syscall：可能的日志级别之一。
% 线程：可能的日志级别之一。
% trx：可能的日志级别之一。
% v8：可能的日志级别之一。
% views：可能的日志级别之一。
% ldap：可能的日志级别之一。
% audit-authentication：可能的日志级别之一。
% audit-authorization：可能的日志级别之一。
% audit-database：可能的日志级别之一。
% audit-collection：可能的日志级别之一。
% audit-view：可能的日志级别之一。
% audit-document：可能的日志级别之一。
% audit-service：可能的日志级别之一。
% 修改并返回服务器的当前日志级别设置。请求主体必须是JSON对象，日志主题为对象键，日志级别为对象值。
% 结果是一个JSON对象，其中调整后的日志主题为对象键，调整后的日志级别为对象值。
% 它可以通过仅将日志级别指定为字符串而不设置json来设置所有功能的日志级别。
% 可能的日志级别为：
% 致命-这是没有办法的。此消息后，ArangoDB将关闭。
% 错误-这是一个错误。您应该调查并修复它。它可能会损害您的生产。
% 警告-这可能是严重的应用程序明智的选择，但我们不知道。
% 信息-发生了什么事，请注意，但没有发生任何戏剧事件。
% 调试-输出调试消息
% 跟踪-跟踪-准备要淹没的日志-请勿在生产中使用。
% 返回码
% 200：如果请求有效，则返回
% 400：当请求正文包含无效JSON时返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果服务器由于内存不足错误而无法生成结果，则返回。
modifyAdminLogLevel(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/log/level">>, ?AgDefQuery, ?AgDefHeader, eVPack:encodeBin(MapData)).

% TLS
% 返回TLS数据的摘要
% 返回此服务器的TLS数据（服务器密钥，客户端身份验证CA）
% GET /_admin/server/tls
% 返回TLS数据的摘要。JSON响应将包含result具有以下组件的字段 ：
% keyfile：有关密钥文件的信息。
% clientCA：有关用于客户端证书验证的CA的信息。
% 如果使用服务器名称指示（SNI），并且为不同的服务器名称配置了多个密钥文件，那么将存在一个附加属性SNI，该属性为每个配置的服务器名称包含有关该服务器名称的密钥文件的相应信息。
%
% 在所有情况下，该属性的值都将是一个JSON对象，该对象具有以下属性的子集（无论适当）：
% SHA256：该值是一个带有整个输入文件的SHA256的字符串。
% certificates：值是一个JSON数组，文件链中包含公共证书。
% privateKeySHA256：如果存在私钥（keyfile 但没有私钥clientCA），则此字段存在并且包含带有私钥SHA256的JSON字符串。
% 这是一个公共API，因此它不要求身份验证。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
getAdminTLS(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/tls">>).

% 触发TLS数据的重新加载并返回摘要
% 触发此服务器的TLS数据（服务器密钥，客户端身份验证CA）的重新加载，并以摘要形式返回新数据。
% POST /_admin/server/tls
% 此API调用触发所有TLS数据的重新加载，然后返回摘要。JSON响应与相应的GET请求完全相同（请参见此处）。
% 这是受保护的API，只能以超级用户权限执行。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
% 403：如果未使用超级用户权限调用此API，它将返回HTTP 403 FORBIDDEN。
triggerAdminTLS(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/server/tls">>).

% 静态加密
% 旋转静态密钥库的加密
% 静态密钥轮换加密
% POST /_admin/server/encryption
% 通过向此端点发送不带负载的请求来更改用户提供的静态加密。通过提供的文件--rocksdb.encryption-keyfolder 将被重新加载，内部加密密钥将使用新的用户密钥重新加密。
% 这是一个受保护的 API，只能以超级用户权限执行。此 API 在协调器节点上不可用。
% 如果禁用加密密钥轮换，API 将返回 HTTP 404。
% HTTP 200如果一切正常，此 API 将返回 HTTP 200
% error : 指示是否发生错误的布尔标志（在这种情况下为false）
% 代码：HTTP 状态代码 - 在这种情况下为 200
% 结果：结果对象。
% 加密密钥：具有密钥秘密的 SHA-256 哈希值的对象数组。可以为空。
% 403：如果不是以超级用户权限调用，此 API 将返回 HTTP 403 FORBIDDEN。
% 404：如果禁用了加密密钥轮换，此 API 将返回 HTTP 404。
encryption(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/server/encryption">>).

% 集群
% 服务器返回是否是在只读模式
% 返回此服务器的模式（只读或默认）
% GET /_admin/server/mode
% 关于服务器的返回模式信息。json响应将包含一个mode值为readonly或的字段default。在只读服务器中，所有写入操作将失败，错误代码为1004（ERROR_READ_ONLY）。创建或删除数据库和集合也将失败，并显示错误代码11（ERROR_FORBIDDEN）。
% 这是一个公共API，因此它不要求身份验证。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
getAdminServerMode(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/mode">>).

% 更新服务器是否处于只读模式
% 更新此服务器的模式（只读或默认）
% PUT /_admin/server/mode
% 需要具有这些属性的 JSON 对象：
% 模式：服务器的模式readonly或default。
% 更新有关服务器的模式信息。json 响应将包含一个mode值为readonlyor的字段default。在只读服务器中，所有写入操作都将失败，错误代码为1004( ERROR_READ_ONLY )。创建或删除数据库和集合也将失败并显示错误代码11( ERROR_FORBIDDEN )。
% 这是一个受保护的 API。它需要身份验证和管理服务器权限。
% 返回代码
% 200：如果一切正常，此 API 将返回 HTTP 200
% 401：如果请求没有被认证为具有足够权限的用户
setAdminServerMode(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/server/mode">>).

% 返回集群永久链接中服务器的ID
% 了解服务器的内部ID
% GET /_admin/server/id
% 返回集群中服务器的ID。如果服务器未在集群模式下运行，则请求将失败。
% 返回码
% 200：当服务器以群集模式运行时返回。
% 500：当服务器未在群集模式下运行时返回。
getAdminServerId(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/id">>).

% 返回集群中服务器的角色
% GET /_admin/server/role
% 返回服务器在集群中的角色。在结果的角色属性中返回角色。角色可能的返回值是：
%    SINGLE：服务器是一个没有集群的独立服务器
%    COORDINATOR：服务器是集群中的协调器
%    PRIMARY：服务器是集群中的数据库服务器
%    SECONDARY：不再使用此角色
%    AGENT：服务器是集群中的代理节点
%    UNDEFINED：在集群中，如果无法确定服务器角色，则返回UNDEFINED。
% 在所有情况下都返回HTTP 200。
% 错误：总是错误
% code : HTTP 状态码，总是 200
% errorNum : 服务器错误号
% 作用：之一[ SINGLE，协调员，PRIMARY，SECONDARY，AGENT，UNDEFINED ]
getAdminServerRole(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/role">>).

% 返回服务器是否可用
% GET /_admin/server/availability
% 返回有关服务器的可用性信息。
% 这是一个公共API，因此它不要求身份验证。它仅在服务器监视的上下文中使用。
% 返回码
% 200：如果服务器已启动并且正在运行并且可用于任意操作，并且未设置为只读模式，并且在活动故障转移设置的情况下当前不是关注者，则此API将返回HTTP 200。
% 503：如果服务器在启动或关闭过程中，设置为只读模式或当前在活动故障转移设置中为关注者，则将返回HTTP 503。
getAdminServerAvailability(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/availability">>).

% DBserver 永久链接的查询统计信息
% 允许查询集群中数据库服务器的统计信息
% GET /_admin/clusterStatistics
% 查询参数
% DBserver（必填）：查询给定DBserver的统计信息
% 返回码
% 200：
% 400：数据库服务器的ID
% 403：
getAdminClusterProps(PoolNameOrSocket, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/clusterStatistics">>, QueryPars).

% 查询集群的运行状况以监视Permalink
% 返回由监督（机构）评估的集群的运行状况
% GET /_admin/cluster/health
% 查询群集的运行状况以进行监视。该响应是一个JSON对象，包含标准code，error，errorNum，和errorMessage字段适当。特定于端点的字段如下：
% ClusterId：标识集群的UUID字符串
% Health：一个对象，该对象包含群集中每个节点的描述性子对象。
% <nodeID>：中的每个条目Health将由节点ID键入，并包含以下属性：
% Endpoint：代表服务器网络端点的字符串。
% Role：服务器扮演的角色。可能的值是"AGENT"，"COORDINATOR"和"DBSERVER"。
% CanBeDeleted：布尔值，表示是否可以安全地从群集中删除节点。
% Version：该节点使用的ArangoDB的版本字符串。
% Engine：该节点使用的存储引擎。
% Status：一个字符串，指示由监督（机构）评估的节点的运行状况。对于协调器和dbservers节点运行状况，应将其视为真实的主要来源。如果节点正常响应请求，则为"GOOD"。如果错过了一个心跳，那就是"BAD"。如果在缺少心跳约15秒钟后通过监督宣布它失败，则会对其进行标记"FAILED"。
% 此外，它还将具有以下属性：
% 协调器和数据库服务器
% SyncStatus：节点上次报告的同步状态。该值主要用于确定的值Status。可能的值包括"UNKNOWN"，"UNDEFINED"，"STARTUP"，"STOPPING"，"STOPPED"，"SERVING"，"SHUTDOWN"。
% LastAckedTime：ISO 8601时间戳记，指定接收到的最后一个心跳。
% ShortName：代表服务器的简称的字符串，例如"Coordinator0001"。
% Timestamp：ISO 8601时间戳记，指定接收到的最后一个心跳。（已弃用）
% Host：可选字符串，指定主机（如果已知）。
% 仅协调员
% AdvertisedEndpoint：表示已播报端点的字符串（如果已设置）。（例如，外部IP地址或负载平衡器，可选）
% 代理商
% Leader：此节点视为领导者的代理的ID。
% Leading：此代理程序是否是领导者（true）或不是（false）。
% LastAckedTime：自上次以来的时间（acked以秒为单位）。
% 返回码
% 200：
getAdminClusterHealth(PoolNameOrSocket) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/cluster/health">>).

% 其他
% 压缩整个数据库系统数据
% PUT /_admin/compact
% 此命令会导致对所有数据库中的所有数据进行完全重写，这对于大型数据库可能需要很长时间。因此，只有在可以长时间承受额外 I/O 负载的情况下，才应谨慎使用它。
% 需要具有这些属性的 JSON 对象：
%    changeLevel : 压缩的数据是否应该移动到最低可能的级别。默认值为false。
%    compactBottomMostLevel : 是否压缩最底层的数据。默认值为false。
%
% 此端点可用于在发生大量数据删除后回收磁盘空间。它需要超级用户访问权限。
% 返回代码
%    200：压缩成功启动
%    401：如果请求没有被认证为具有足够权限的用户
compactSystemData(PoolNameOrSocket) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/compact">>).

% 重新加载路由表。
% POST /_admin/routing/reload
% 从集合路由中重新加载路由信息。
% 返回码
% 200：路由信息重新加载成功
reloadAdminRouting(PoolNameOrSocket) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/routing/reload">>).


% 指标 API v2
% 返回当前实例指标
% GET /_admin/metrics/v2
% 查询参数
% serverId（可选）：返回指定服务器的指标。如果没有给出 serverId，被询问的服务器将回复。这个参数只对 Coordinator 有意义。
% 以 Prometheus 格式返回实例的当前指标。返回的文档收集所有实例指标，这些指标在任何给定时间进行测量，并将它们公开以供 Prometheus 收集。
% 该文档包含不同的指标和指标组，具体取决于所查询实例的角色。所有导出的指标都使用前缀发布，arangodb_以将它们与其他收集的数据区分开来。
% 然后需要将 API 添加到 Prometheus 配置文件中进行收集。
% 返回代码
% 200：成功返回指标。
% 404：可以使用--server.export-metrics-api false 服务器中的设置禁用指标 API 。在这种情况下，调用的结果表明找不到 API。
metricsV2(PoolNameOrSocket) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/metrics/v2">>).

metricsV2(PoolNameOrSocket, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/metrics/v2">>, QueryPars).
