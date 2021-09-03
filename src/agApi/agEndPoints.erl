-module(agEndPoints).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/endpoints.html

% 端点的HTTP接口
% 该API /_api/endpoint已弃用。对于集群模式，/_api/cluster/endpoints可以找到所有当前的Coordinator端点（请参见下文）。
%
% ArangoDB服务器可以在多个端点上侦听传入的请求。
%
% 通常使用“ --server.endpoint”选项在ArangoDB的配置文件或命令行中指定端点。ArangoDB的默认终结点是tcp：//127.0.0.1：8529或 tcp：// localhost：8529。
%
% 请注意，所有端点管理操作只能通过默认数据库（_system）访问，而不能通过其他任何数据库访问。

% 获取有关所有协调器端点的信息固定链接
% 此API调用返回有关所有协调器终结点的信息（仅集群）。
% GET /_api/cluster/endpoints
% 返回带有属性endpoints的对象，该对象包含一个对象数组，每个对象都有一个属性endpoint，其值是带有端点描述的字符串。集群中的每个协调器都有一个条目。此方法仅适用于群集模式下的协调器。如果发生错误，则将error属性设置为 true。
% HTTP 200
% error：布尔值标志，指示是否发生错误（在这种情况下为true）
% code：HTTP状态码-200
% 端点：活动集群端点的列表。
% 端点：协调器的绑定，例如tcp://[::1]:8530
% 501：
getClusterEndpoints(PoolNameOrSocket) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/cluster/endpoints">>, ?AgDefQuery, ?AgDefHeader, ?AgDefBody, true).