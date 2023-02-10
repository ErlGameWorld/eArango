-module(agIndexes).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/indexes.html

% 索引永久链接
% 这是对 ArangoDB 的索引 HTTP 接口的一般介绍。有各种索引类型的特殊部分。
% 索引
% 索引用于允许快速访问文档。对于每个集合，总是有主索引，它是文档键（_key 属性）的哈希索引 。不能删除或更改此索引。 边缘集合还将具有自动创建的边缘索引，该索引无法修改。该索引通过_from和_to属性提供对文档的快速访问。
% 大多数用户空间索引可以通过定义应该被索引的属性的名称来创建。一些索引类型只允许索引一个属性（例如全文索引），而其他索引类型允许索引多个属性。
% _id任何索引类型都不支持在用户定义的索引中使用系统属性。
% 索引句柄
%    索引句柄唯一标识数据库中的索引。它是一个字符串，由集合名称和以 / 分隔的索引标识符组成。如果索引被声明为唯一的，那么对索引属性的访问应该很快。如果索引属性仅包含很少的不同值，则性能会下降。
% 一级索引
%    为每个集合自动创建一个主索引。它索引文档的主键，这些主键存储在_key系统属性中。主索引是唯一的，可用于查询_key和_id属性。无法显式创建或删除主索引。
% 边缘索引
%    为边集合自动创建边索引。它包含顶点文档之间的连接，并在查询顶点的连接边时调用。无法显式创建或删除边缘索引。边缘索引是非唯一的。
% 哈希索引
%    哈希索引是一种未排序的索引，可用于通过等式查找来查找单个文档。
% 跳过列表索引
%    跳过列表是一种排序索引，可用于查找单个文档或文档范围。
% 持久索引
%    持久索引是一种排序索引，可用于查找单个文档或文档范围。与其他索引相比，持久索引的内容存储在磁盘上，因此在加载集合时不需要在内存中从文档中重建。
% TTL（生存时间）索引
%    TTL 索引可用于自动从集合中删除过期文档。过期的文档最终会被后台线程删除。
% 全文索引
%    全文索引可用于在文档中查找单词或单词前缀。全文索引只能在一个属性上设置，并将索引包含在该属性中具有文本值的文档中的所有单词。只有具有（可指定的）最小长度的词才会被索引。单词标记化是使用 libicu 提供的单词边界分析完成的，它考虑了服务器启动时提供的所选语言。单词以其小写形式索引。索引支持全匹配查询（全词）和前缀查询。
% 索引地址
%    ArangoDB 中的所有索引都有一个唯一的句柄。此索引句柄标识索引并由 ArangoDB 管理。所有索引都在 URI 下找到
%    http://server:port/_api/index/index-handle
% 例如：假设索引句柄是demo/63563528那么该索引的 URL 是：
%    http://localhost:8529/_api/index/demo/63563528

% 返回索引
% GET /_api/index/{index-id}
% 路径参数
%    index-id（必需）：索引标识符。
% 结果是一个描述索引的对象。它至少具有以下属性：
%    id：索引的标识符
%    type：索引类型
% 所有其他属性都取决于类型。例如，某些索引提供 唯一或稀疏标志，而另一些则不提供。一些索引还在结果的selectivityEstimate属性中提供了选择性估计。
% 返回码
%    200：如果索引存在，则返回HTTP 200。
%    404：如果索引不存在，则 返回HTTP 404。
getIndexInfo(PoolNameOrSocket, IndexId) ->
	Path = <<"/_api/index/", (agMiscUtils:toBinary(IndexId))/binary>>,
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, Path).

% 创建一个索引
% POST /_api/index#general
% 查询参数
%    collection （必填）：集合名称。
% 请求正文（json）
% 在集合collection中创建一个新索引。期望包含索引详细信息的对象。
% 必须在 索引详细信息的type属性中指定要创建的索引的类型。根据索引类型，可能需要在请求中指定其他其他属性才能创建索引。
% 索引要求索引详细信息的fields属性中的被索引属性。根据索引类型，可以为一个或多个属性建立索引。在后一种情况下，需要一个字符串数组。
% 用户定义的索引不支持索引系统属性_id。使用_id作为索引属性手动创建索引将失败，并显示错误。
% （可选）索引名称可以在name属性中指定为字符串。索引名称与集合名称具有相同的限制。如果未指定任何值，将自动生成一个值。
% 某些索引可以创建为唯一或非唯一变体。通过在索引详细信息中指定唯一标志，可以控制大多数索引的唯一性。将其设置为true将创建唯一索引。将其设置为false或忽略unique属性将创建一个非唯一索引。
% 注意：以下索引类型不支持唯一性，并且对这些类型使用unique属性可能会导致错误：
%     地理索引
%     全文索引
% 注意：集群中不支持非分片键上的唯一索引。
% 可以选择在稀疏变量中创建哈希，跳过列表和持久索引。如果索引详细信息中的sparse属性设置为true，则将创建一个稀疏索引。稀疏索引不会索引未设置任何索引属性或为null的文档。
%
% 类型为hash或skiplist的数组索引支持可选的重复数据删除属性。它控制将来自同一文档的重复索引值插入唯一数组索引是否会导致唯一约束错误。默认值为true，因此每个非唯一索引值的单个实例将插入每个文档的索引中。无论此属性的值如何，尝试将值插入到索引中已存在的索引始终将失败。
%
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    400：如果发布了无效的索引描述或使用了目标索引不支持的属性，则返回HTTP 400。
%    404：如果集合未知，则返回HTTP 404。
newIndex(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 删除索引
% DELETE /_api/index/{index-id}
% 路径参数
%    index-id（必需）：索引ID。
% 删除带有index-id的索引。
% 返回码
%    200：如果可以删除索引，则返回HTTP 200。
%    404：如果index-id未知，则返回HTTP 404。
delIndex(PoolNameOrSocket, IndexId) ->
	Path = <<"/_api/index/", (agMiscUtils:toBinary(IndexId))/binary>>,
	agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path).

% 返回集合的所有索引
% GET /_api/index
% 查询参数
%    collection （必填）：集合名称。
% 返回一个对象，该对象的属性索引包含给定集合的所有索引描述的数组。在标识符中还可以使用与索引句柄作为键的对象相同的信息。
% 返回码
%    200：返回一个JSON对象，其中包含该集合的索引列表。
indexList(PoolNameOrSocket, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/index">>, QueryPars).

% 使用哈希索引
% 如果存在合适的哈希索引，/_api/simple/by-example则将使用该索引执行示例查询。

% 创建一个哈希索引
% POST /_api/index#hash
% 查询参数
%     collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%    type：必须等于“ hash”。
%    fields：属性路径的数组。
%    unique：如果为true，则创建一个唯一索引。
%    sparse：如果为true，则创建一个稀疏索引。
%    deduplicate：如果为false，则关闭数组值的重复数据删除。
% 如果不存在，则为集合collection-name创建哈希索引。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即field）或在任何指定索引属性中都为null的文档将从索引中排除。如果设置了唯一标志，则不会对此类文档建立索引，也不会将其用于唯一性检查。
% 在非稀疏索引中，将为这些文档建立索引（对于不存在索引的属性，将使用null值），并且如果设置了唯一标志，则将对它们进行唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    400：如果集合中已经包含文档，并且您尝试创建唯一哈希索引以使某些文档违反唯一性，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfHash(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 使用跳过列表索引
% 如果存在合适的跳过列表索引，则/_api/simple/range和其他操作将使用该索引来执行查询。
% 创建跳过列表
% 创建一个跳过列表
% POST /_api/index#skiplist
% 查询参数
%    collection（必填）：集合名称。
% 需要具有这些属性的 JSON 对象：
%    type : 必须等于“skiplist”。
%    fields：属性路径数组。
%    unique : 如果为true，则创建唯一索引。
%    sparse：如果为true，则创建一个稀疏索引。
%    deduplicate：如果为false，则关闭数组值的重复数据删除。
% 为集合collection-name创建一个跳过列表索引（如果它不存在）。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即fields）或在任何指定索引属性中具有null值的文档都将从索引中排除。如果设置了唯一标志，此类文档将不会被编入索引，并且不会被考虑用于唯一性检查。
% 在非稀疏索引中，这些文档将被索引（对于不存在的索引属性，将使用null值）并且如果设置了唯一标志，则将考虑唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回代码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引不存在并且可以创建，则 返回HTTP 201。
%    400：如果集合已经包含文档，并且您尝试以存在违反唯一性的文档的方式创建唯一的跳过列表索引，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfSkipList(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 使用持久索引
% 如果存在合适的持久索引，则/_api/simple/range其他操作将使用该索引执行查询。
% 创建持久索引
% POST /_api/index#persistent
% 查询参数
%    集合（必填）：集合名称。
% 需要具有这些属性的 JSON 对象：
%    type : 必须等于“persistent”。
%    fields：属性路径数组。
%    unique : 如果为true，则创建唯一索引。
%    sparse：如果为true，则创建一个稀疏索引。
%    deduplicate：属性deduplicate由类型persistent、 hash或skiplist 的数组索引支持。它控制将来自同一文档的重复索引值插入到唯一数组索引中是否会导致唯一约束错误。默认值为true，因此只有每个非唯一索引值的单个实例会插入到每个文档的索引中。尝试将值插入索引中已存在的索引将始终失败，无论此属性的值如何。
%    estimates：属性estimates由持久类型的索引支持。此属性控制是否为索引维护索引选择性估计。不保持索引选择性估计会对写入性能产生轻微的积极影响。关闭索引选择性估计的缺点是当有多个候选索引可供选择时，查询优化器将无法确定 AQL 查询中不同竞争索引的有用性。该估计属性是可选的，默认为真，如果没有设置。除了持久性（带有哈希和跳过列表）之外，它不会对索引产生任何影响 现在只是持久化的别名）。
% 如果集合collection-name尚不存在，则为该集合创建持久索引。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即fields）或在任何指定索引属性中具有null值的文档都将从索引中排除。如果设置了唯一标志，此类文档将不会被编入索引，并且不会被考虑用于唯一性检查。
% 在非稀疏索引中，这些文档将被索引（对于不存在的索引属性，将使用null值）并且如果设置了唯一标志，则将考虑唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回代码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引不存在并且可以创建，则 返回HTTP 201。
%    400：如果集合已经包含文档，并且您尝试以存在违反唯一性的文档的方式创建唯一的持久索引，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfPersistent(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 使用TTL（生存时间）索引
%
% 创建一个TTL（生存时间）索引
% POST /_api/index#ttl
% 查询参数
% collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%     type：必须等于“ ttl”。
%     fields：一个具有唯一属性路径的数组。
%     expireAfter：文档创建后经过的时间（以秒为单位），之后文档被视为“过期”。
% 为集合collection-name创建TTL索引（如果尚不存在）。该调用需要一个包含索引详细信息的对象。
% 返回码
%     200：如果索引已经存在，则返回HTTP 200。
%     201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%     400：如果集合已经包含另一个TTL索引，则返回HTTP 400，因为每个集合最多可以有一个TTL索引。
%     404：如果集合名称未知，则返回HTTP 404。
newIndexOfTtl(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 创建地理索引
% POST /_api/index#geo
% 查询参数
%       collection （必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%       type：必须等于“ geo”。
%       fields：具有一个或两个属性路径的数组。
%           如果它是一个具有一个属性路径location的数组，那么将使用location作为坐标的路径在所有文档上创建地理空间索引。该属性的值必须是具有至少两个double值的数组。数组必须包含纬度（第一个值）和经度（第二个值）。所有没有属性路径或值不适合的文档都将被忽略。
%            如果它是具有两个属性路径latitude和经度的数组，则将使用纬度 和经度在所有文档上创建地理空间索引作为路径的纬度和经度。属性纬度和属性经度的值必须加倍。所有没有属性路径或值不适合的文档都将被忽略。
%     geoJson：如果在某个位置上构建了地理空间索引，并且geoJson为true，则数组内的顺序为经度和纬度。这对应于http://geojson.org/geojson-spec.html#positions中描述的格式
% 在集合collection-name中创建地理空间索引（如果尚不存在）。期望包含索引详细信息的对象。
% 地理位置索引总是稀疏的，这意味着不包含索引属性或索引属性中具有非数字值的文档将不会被索引。
%
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfGeo(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).

% 全文
% 如果存在全文索引， /_api/simple/fulltext则将使用该索引执行指定的全文查询。
% 创建全文索引
%    POST /_api/index#fulltext
% 查询参数
%    collection（必填）：集合名称。
% 需要具有这些属性的 JSON 对象：
%    type : 必须等于“fulltext”。
%    fields：属性名称数组。目前，该数组仅限于一个属性。
%    minLength：要索引的单词的最小字符长度。如果未指定，将默认为服务器定义的值。因此，建议在创建索引时明确设置此值。
% 如果集合collection-name尚不存在，则为该集合创建全文索引。该调用需要一个包含索引详细信息的对象。
% 返回代码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引不存在并且可以创建，则 返回HTTP 201。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfFulltext(PoolNameOrSocket, MapData, QueryPars) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/index">>, QueryPars, ?AgDefHeader, eVPack:encode(MapData)).