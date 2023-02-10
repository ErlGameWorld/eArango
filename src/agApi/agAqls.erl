-module(agAqls).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:
%     AQL Query Cursors:
%           https://www.arangodb.com/docs/stable/http/aql-query-cursor.html
%     AQL Query:
%           https://www.arangodb.com/docs/stable/http/aql-query.html
%     AQL Query Results Cache:
%           https://www.arangodb.com/docs/stable/http/aql-query-cache.html
%     AQL User Functions Management:
%           https://www.arangodb.com/docs/stable/http/aql-user-functions.html
% 该模块汇总封装上面所有AQL操作

% AQL查询游标的HTTP接口
% 这是ArangoDB查询的HTTP接口的简介。AQL的结果和简单查询作为游标返回，以便批量处理服务器与客户端之间的通信。每次调用将返回一批文档，并指示当前批是否为最终批。根据查询的不同，结果集中的文档总数可能会或可能不会事先知道。为了释放服务器资源，客户端应在不再需要游标时将其删除。
% 要执行查询，需要通过HTTP POST请求将查询详细信息从客户端传送到服务器。

% 检索查询结果
% 选择查询在服务器上即时执行，结果集将返回给客户端。
% 客户端可以通过两种方式从服务器获取结果集：
%    单次返回
%    使用游标

% 单次返回
% 服务器将在一次往返中仅将一定数量的结果文档传送回客户端。客户端可以通过在发出查询时设置batchSize属性来控制此数字。
% 如果完整的结果可以一次性传递给客户端，则客户端不需要发出任何其他请求。客户端可以通过检查结果集的hasMore属性来检查是否已检索到完整的结果集。如果将其设置为false，则客户端已从服务器获取了完整的结果集。在这种情况下，将不会创建服务器端游标。

% 使用游标
% 如果结果集中包含的文档数量超出单次往返传输的文档数量（即通过batchSize属性设置的数量），则服务器将返回前几个文档并创建一个临时游标。游标标识符也将返回给客户端。服务器会将光标标识符放在响应对象的id属性中。此外， 响应对象的hasMore属性将设置为true。这表明客户端还有其他结果要从服务器获取。

% 修改文件
%    该_api/cursor终端还可以用于执行修改查询。
%    下面的示例将一个值附加到集合中arrayValue具有键的文档数组中。正常的更新行为是完全替换属性，并使用带有 函数的更新 AQL 查询允许附加到数组。testdocumentsPUSH()

% 设置内存限制
%    要为查询设置内存限制，可以将memoryLimit选项传递给服务器。内存限制指定允许查询使用的最大字节数。当单个 AQL 查询达到指定的限制值时，查询将因超出资源限制异常而中止。在集群中，内存记帐是按服务器完成的，因此限制值实际上是每个服务器每个查询的内存限制。
%    如果未指定内存限制，则服务器默认值（由启动选项控制--query.memory-limit将用于限制查询可以使用的最大内存量。内存限制值0表示最大量查询的内存不受限制。

% 通过 HTTP 访问游标
% 创建游标并返回第一个结果
% POST /_api/cursor
% 描述查询和查询参数的 JSON 对象。
% 需要具有这些属性的 JSON 对象：
%    query : 包含要执行的查询字符串
%    count：指示是否应在结果的“count”属性中返回结果集中的文档数。计算“count”属性可能会对将来的某些查询产生性能影响，因此默认情况下关闭此选项，并且仅在请求时返回“count”。
%    batchSize : 在一次往返中从服务器传输到客户端的最大结果文档数。如果未设置此属性，则将使用服务器控制的默认值。甲BATCHSIZE的值 0是不允许的。
%    ttl：光标的生存时间（以秒为单位）。如果结果集足够小（小于或等于batchSize），则立即返回结果。否则，它们将存储在内存中，并且可以通过相对于ttl. 在指定的时间后，光标将在服务器上自动移除。这有助于确保对客户端未完全获取的游标进行垃圾回收。如果未设置，将使用服务器定义的值（默认值：30 秒）。
%    cache : 用于确定是否应使用 AQL 查询结果缓存的标志。如果设置为false，则将跳过查询的任何查询缓存查找。如果设置为true，如果查询缓存模式是on或demand，它将导致为查询检查查询缓存。
%    memoryLimit：允许查询使用的最大内存数（以字节为单位）。如果设置，则查询将失败并显示错误“超出资源限制”，以防它分配太多内存。值0表示没有内存限制。
%    bindVars：表示绑定参数的键/值对。
%    options：具有查询额外选项的键/值对象。
%    fullCount：如果设置为true并且查询包含LIMIT子句，则结果将有一个额外的属性，包含子属性stats 和fullCount , { ... , "extra": { "stats": { "fullCount": 123 } } }。该FULLCOUNT属性将包含的文档数量的结果应用于在查询的最后顶层限制之前。它可用于计算符合特定过滤条件的文档数量，但一次只返回其中的一个子集。因此它类似于 MySQL 的SQL_CALC_FOUND_ROWS暗示。请注意，设置该选项将禁用一些 LIMIT 优化，并可能导致处理更多文档，从而使查询运行时间更长。请注意，如果查询具有顶级 LIMIT 子句并且查询中实际使用了 LIMIT 子句，则fullCount属性可能仅出现在结果中。
%    maxPlans：限制 AQL 查询优化器创建的最大计划数。
%    maxWarningCount：限制查询将返回的最大警告数。默认情况下，查询将返回的警告数量限制为 10，但可以通过设置此属性来增加或减少该数量。
%    failOnWarning：当设置为true 时，查询将抛出异常并中止而不是产生警告。在开发过程中应使用此选项以尽早发现潜在问题。当该属性设置为false 时，警告不会传播到异常，并将与查询结果一起返回。还有一个服务器配置选项，--query.fail-on-warning用于设置failOnWarning的默认值，因此不需要在每个查询级别进行设置。
%    stream : 可以启用延迟执行查询。如果设置为true，则只要需要，就会执行查询以产生最多batchSize结果。这些结果会立即返回并暂停查询，直到客户端请求下一批（如果有更多结果）。根据查询，这可能意味着第一个结果将更快地可用并且需要更少的内存，因为服务器一次只需要存储结果的一个子集。只读查询可以受益最大，除非SORT 没有索引或COLLECT涉及必须在返回部分结果之前处理所有文档。建议仅将此选项用于没有排他锁的查询。
%
% 评论：
% 查询将保留资源直到它结束（例如 RocksDB 快照，这在一定程度上防止了压缩）。写入将在内存中，直到提交查询。
% 如果现有文档被修改，那么这些文档上的写锁就会被持有，并且其他试图修改相同文档的查询将因为这个冲突而失败。
% 在某些批次已经成功返回之后，由于冲突或其他原因，流式查询可能会延迟失败，这可能会使到该点的结果变得毫无意义。
% 查询选项cache，count并且fullCount不支持流媒体的查询。
% 查询统计信息、分析数据和警告作为最后一批的一部分提供。
% 如果该stream选项为false（默认），则在将查询的任何结果返回给客户端之前计算查询的完整结果。服务器将完整结果存储在内存中（如果在集群中，则在联系的协调器上）。所有其他资源都会立即释放（锁、RocksDB 快照）。如果发生冲突，查询将在返回结果之前失败。
%    optimizer：与查询优化器相关的选项。
%    rules：可以将要包含或要排除的优化器规则列表放入此属性中，告诉优化器包含或排除特定规则。要禁用规则，请为其名称添加前缀-，要启用规则，请为其添加前缀+。还有一个伪规则all，它匹配所有优化器规则。-all禁用所有规则。
%    profile：如果设置为true或1，则如果查询结果不是从查询缓存中提供的，则附加查询分析信息将在额外返回属性的子属性配置文件中返回。设置为2查询将在额外返回属性的子属性stats.nodes中包括每个查询计划节点的执行统计信息。此外，查询计划在子属性extra.plan 中返回。
%    SatelliteSyncWait：这个企业版参数允许配置 DB-Server 将有多长时间将查询中涉及的 SatelliteCollections 同步。默认值为60.0（秒）。当达到最大时间时，查询将停止。
%    maxRuntime：查询必须在给定的运行时内执行，否则将被终止。该值以秒为单位指定。默认值为0.0（无超时）。
%    maxTransactionSize：以字节为单位的交易大小限制。仅由 RocksDB 存储引擎授予。
%    middleCommitSize：最大操作总大小，之后自动执行中间提交。仅由 RocksDB 存储引擎授予。
%    middleCommitCount：在自动执行中间提交之后的最大操作数。仅由 RocksDB 存储引擎授予。
%    skipInaccessibleCollections：AQL 查询（尤其是图遍历）会将用户没有访问权限的集合视为这些集合是空的。您的查询将正常执行，而不是返回禁止访问错误。这旨在帮助某些用例：一个图包含多个集合，不同的用户在该图上执行 AQL 查询。您现在可以通过更改用户对集合的访问权限来自然地限制可访问的结果。此功能仅在企业版中可用。
%
% 查询详细信息包括查询字符串以及可选的查询选项和绑定参数。这些值需要在 POST 请求正文中以 JSON 表示形式传递。
% 如果服务器可以创建结果集，则返回HTTP 201。
% error：指示发生错误的标志（在本例中为false）
% code : HTTP 状态码
% result : 结果文档数组（如果查询没有结果，则可能为空）
% hasMore：一个布尔值指示符是否有更多结果可用于服务器上的游标
% count：可用的结果文档总数（仅当使用count属性集执行查询时可用）
% id : 在服务器上创建的临时游标的 id（可选，见上文）
% extra：一个可选的 JSON 对象，其中包含有关包含在其stats子属性中的查询结果的额外信息。对于数据修改查询， extra.stats子属性将包含修改的文档数和由于错误而无法修改的文档数（如果指定了ignoreErrors查询选项）
% cached：一个布尔标志，指示查询结果是否来自查询缓存。如果查询结果是从查询缓存中提供的，额外的返回属性将不包含任何stats子属性，也没有profile子属性。
% 如果 JSON 表示格式错误或请求中缺少查询规范，则返回HTTP 400。
% 如果 JSON 表示格式错误或请求中缺少查询规范，服务器将使用HTTP 400 进行响应。
% 响应的正文将包含一个带有附加错误详细信息的 JSON 对象。该对象具有以下属性：
% error : 指示发生错误的布尔标志（在这种情况下为true）
% code : HTTP 状态码
% errorNum : 服务器错误号
% errorMessage：描述性错误消息
% 如果查询规范完成，服务器将处理查询。如果在查询处理过程中发生错误，服务器将响应HTTP 400。同样，响应的正文将包含有关错误的详细信息。
% 404：如果在查询中访问了不存在的集合，服务器将使用HTTP 404进行响应。
% 405：如果使用了不受支持的 HTTP 方法，服务器将使用HTTP 405 进行响应。
newCursor(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/cursor">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% 从现有游标返回下一个结果
% PUT /_api/cursor/{cursor-identifier}
% 路径参数
% cursor-identifier（必填）：光标的名称
% 如果游标仍然存在，则返回具有以下属性的对象：
% id：光标标识符
% result：当前批次的文档列表
% hasMore：如果这是最后一批，则为false
% count：如果存在，元素总数
% 请注意，即使hasMore返回true，下一次调用仍可能不返回任何文档。但是，如果hasMore为false，则光标将被耗尽。一旦hasMore属性的值为 false，客户端就可以停止。
% 返回码
% 200：如果成功，服务器将以HTTP 200响应。
% 400：如果省略了光标标识符，则服务器将使用HTTP 404进行响应。
% 404：如果找不到具有指定标识符的游标，则服务器将使用HTTP 404进行响应。
nextCursor(PoolNameOrSocket, CursorId) ->
   Path = <<"/_api/cursor/", (agMiscUtils:toBinary(CursorId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, Path).

% 删除光标永
% DELETE /_api/cursor/{cursor-identifier}
% 路径参数
%    cursor-identifier（必填）：光标的ID
% 删除游标并释放与其关联的资源。
% 当客户端从服务器上检索所有文档时，游标将在服务器上自动销毁。客户端还可以使用HTTP DELETE请求在任何较早的时间显式销毁游标。游标ID必须作为URL的一部分包含在内。
% 注意：在服务器控制的特定超时后，服务器还将自动销毁废弃的游标，以避免资源泄漏。
% 返回码
%    202：如果服务器知道游标，则返回。
%    404：如果服务器不知道游标，则返回404。如果在销毁游标后使用了游标，也将返回该值。
delCursor(PoolNameOrSocket, CursorId) ->
   Path = <<"/_api/cursor/", (agMiscUtils:toBinary(CursorId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path).

% AQL查询的HTTP接口
%
% 解释和解析查询
% ArangoDB有一个HTTP接口，用于语法验证AQL查询。此外，它提供了一个HTTP接口来检索任何有效AQL查询的执行计划。
% 这两个功能实际上并不执行提供的AQL查询，而只是检查它并返回有关它的元信息。

% 解释 AQL 查询并返回有关它的信息
% POST /_api/explain
% 描述查询和查询参数的 JSON 对象。
%    query：您要解释的查询；如果查询引用了任何绑定变量，则这些变量也必须在属性bindVars 中传递。可以在options属性中传递查询的其他选项。
%    bindVars：表示绑定参数的键/值对。
%    options : 查询选项
%       allPlans：如果设置为true，则将返回所有可能的执行计划。默认值为false，这意味着只会返回最佳计划。
%       maxNumberOfPlans：允许优化器生成的可选最大计划数。将此属性设置为较低的值可以限制优化器所做的工作量。
%       optimizer：与查询优化器相关的选项。
%       rules：可以将要包含或要排除的优化器规则列表放入此属性中，告诉优化器包含或排除特定规则。要禁用规则，请为其名称添加前缀-，要启用规则，请为其添加前缀+。还有一个伪规则all，它匹配所有优化器规则。-all禁用所有规则。
%
% 为了解释如何在服务器上执行 AQL 查询，可以通过 HTTP POST 请求将查询字符串发送到服务器。然后服务器将验证查询并为其创建执行计划。会返回执行计划，但不会执行查询。
% 服务器返回的执行计划可用于估计查询的可能性能。尽管实际性能将取决于许多不同的因素，但执行计划通常可以粗略估计服务器为实际运行查询所需的工作量。
% 默认情况下，解释操作将返回查询优化器选择的最优计划。最优计划是总估计成本最低的计划。计划将在响应对象的属性计划中返回。如果在请求中指定了allPlans选项，则结果将包含优化器创建的所有计划。这些计划将在属性计划中返回。
% 结果还将包含一个属性warnings，它是在优化或执行计划创建期间发生的警告数组。此外，结果中包含一个stats属性以及一些优化器统计信息。如果allPlans设置为false，则结果将包含一个属性cacheable ，该属性说明如果使用查询结果缓存，查询结果是否可以缓存在服务器上。当allPlans 设置为true时，cacheable属性不存在。
% 结果中的每个计划都是一个具有以下属性的 JSON 对象：
% nodes：计划的执行节点数组。
% 估计成本：计划的总估计成本。如果有多个计划，优化器会选择总成本最低的计划。
% collections : 查询中使用的集合数组
% rules : 优化器应用的一系列规则。
% variables：查询中使用的变量数组（注意：这可能包含优化器创建的内部变量）
% 返回代码
% 200：如果查询有效，服务器会响应HTTP 200，并在响应的plan属性中返回最优执行计划。如果在请求中设置了allPlans选项，则会在allPlans属性中返回一个计划数组。
% 400：如果请求格式错误，或者查询包含解析错误，服务器将使用HTTP 400响应。响应的正文将包含嵌入在 JSON 对象中的错误详细信息。如果查询引用任何绑定变量，则省略绑定变量也会导致HTTP 400错误。
% 404：如果在查询中访问了不存在的集合，服务器将使用HTTP 404进行响应。
explainQuery(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/explain">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% 解析一个AQL查询并返回有关它的信息
% POST /_api/query
% 具有以下属性的JSON对象是必需的：
%    query：要在不执行查询字符串的情况下对其进行验证，可以通过HTTP POST请求将查询字符串传递到服务器。
% 该端点仅用于查询验证。要实际查询数据库，请参阅/api/cursor。
% 返回码
%    200：如果查询有效，服务器将使用HTTP 200进行响应，并在响应的bindVars属性中返回在查询中找到的绑定参数的名称（如果有）。它还将在collections属性中返回查询中使用的collections的数组。如果查询可以成功解析，则返回的JSON 的ast属性将包含查询的抽象语法树表示形式。ast的格式在将来的ArangoDB版本中可能会发生变化，但是可以用来检查ArangoDB如何解释给定查询。请注意，将在不对其应用任何优化的情况下返回抽象语法树。
%    400：如果请求格式错误或查询包含解析错误，服务器将以HTTP 400响应。响应的正文将包含嵌入在JSON对象中的错误详细信息。
parseQuery(PoolNameOrSocket, MapData) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/query">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).


% 返回所有 AQL 优化器规则
% 返回 AQL 查询的所有可用优化器规则的列表。
% GET /_api/query/rules
% 所有优化器规则及其属性的列表。
% 回应
% HTTP 200：如果可以成功检索到优化器规则列表，则返回。
% （数组）：对象数组。每个对象描述一个 AQL 优化器规则。
% 名称（字符串）：查询解释输出中显示的优化器规则的名称。
% flags（对象）：具有规则属性的对象。
%     hidden (boolean): 规则是否显示给用户。内部规则是隐藏的。
%     clusterOnly (boolean)：规则是否仅适用于集群部署模式。
%     canBeDisabled（布尔值）：是否允许用户禁用此规则。一些规则是强制性的。
%     canCreateAdditionalPlans (boolean)：此规则是否可以创建额外的查询执行计划。
%     disabledByDefault (boolean)：优化器是否默认考虑这个规则。
%     enterpriseOnly（布尔值）：规则是否仅在企业版中可用。
getRules(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/rules">>).

% 查询跟踪固定链接
% ArangoDB具有HTTP接口，用于检索当前正在执行的AQL查询列表和慢速AQL查询列表。为了有意义地使用这些API，需要在执行HTTP请求的数据库中启用查询跟踪。

% 返回AQL查询跟踪的配置
% GET /_api/query/properties
% 返回当前查询跟踪配置。该配置是具有以下属性的JSON对象：
%     enabled：如果设置为true，那么将跟踪查询。如果设置为 false，则不会跟踪查询或慢速查询。
%     trackSlowQueries：如果设置为true，则如果慢查询的运行时间超过了slowQueryThreshold中设置的值，则将在慢查询列表中跟踪慢查询 。为了跟踪慢查询， 还必须将enabled属性设置为true。
%     trackBindVars：如果设置为true，那么将跟踪查询中使用的绑定变量。
%     maxSlowQueries：保留在慢速查询列表中的最大慢速查询数。如果慢速查询列表已满，则在发生其他慢速查询时，其中最早的条目将被丢弃。
%     slowQueryThreshold：用于将查询视为慢查询的阈值。当启用慢查询跟踪时，运行时大于或等于此阈值的查询将被放入慢查询列表中。slowQueryThreshold的值以秒为单位指定。
%     maxQueryStringLength：保留在查询列表中的最大查询字符串长度。查询字符串可以有任意长度，如果使用非常长的查询字符串，则可以使用此属性来节省内存。该值以字节为单位指定。
% 返回码
% 200：如果成功检索到属性，则返回。
% 400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQueryProps(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/properties">>).

% 更改AQL查询跟踪的配置
% PUT /_api/query/properties
% 具有以下属性的JSON对象是必需的：
%     enabled：如果设置为true，那么将跟踪查询。如果设置为 false，则不会跟踪查询或慢速查询。
%     trackSlowQueries：如果设置为true，则如果慢查询的运行时间超过了slowQueryThreshold中设置的值，则将在慢查询列表中跟踪慢查询 。为了跟踪慢查询， 还必须将enabled属性设置为true。
%     trackBindVars：如果设置为true，那么将与查询一起跟踪查询中使用的绑定变量。
%     maxSlowQueries：保留在慢速查询列表中的最大慢速查询数。如果慢速查询列表已满，则在发生其他慢速查询时，其中最早的条目将被丢弃。
%     slowQueryThreshold：用于将查询视为慢速的阈值。当启用慢查询跟踪时，运行时大于或等于此阈值的查询将被放入慢查询列表中。slowQueryThreshold的值以秒为单位指定。
%     maxQueryStringLength：要保留在查询列表中的最大查询字符串长度。查询字符串可以有任意长度，如果使用非常长的查询字符串，则可以使用此属性来节省内存。该值以字节为单位指定。
% 这些属性需要在HTTP请求主体的属性属性中传递。属性必须是JSON对象。
% 更改属性后，将在HTTP响应中返回当前属性集。
% 返回码
%     200：如果属性更改成功，则返回。
%     400：如果请求格式错误，服务器将以HTTP 400进行响应，
changeQueryProps(PoolNameOrSocket, MapData) ->
	agVstCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/query/properties">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% 返回当前运行的 AQL 查询
% GET /_api/query/current
% 查询参数
%    all（可选）：如果设置为true，将返回所有数据库中当前正在运行的查询，而不仅仅是选定的查询。仅允许在系统数据库中使用该参数并具有超级用户权限。
% 返回一个包含当前在所选数据库中运行的 AQL 查询的数组。每个查询都是一个 JSON 对象，具有以下属性：
%    id : 查询的 id
%    database : 运行查询的数据库的名称
%    user : 开始查询的用户名
%    query : 查询字符串（可能被截断）
%    bindVars：查询使用的绑定参数值
%    started：查询开始的日期和时间
%    runTime：查询的运行时间直到查询列表被查询
%    state：查询的当前执行状态（作为字符串）。之一：
%       "initializing"
%       "parsing"
%       "optimizing ast"
%       "loading collections"
%       "instantiating plan"
%       "optimizing plan"
%       "executing"
%       "finalizing"
%       "finished"
%       "killed"
%       "invalid"
%    stream : 查询是否使用流式游标
% 返回代码
% 200：查询列表检索成功时返回。
% 400：如果请求格式错误，服务器将响应HTTP 400，
% 403：如果使用了all参数，但请求是在与 _system 不同的数据库中发出的，或者由非特权用户发出的，则返回HTTP 403。
currentQuery(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/current">>).

currentQuery(PoolNameOrSocket, QueryPars) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/current">>, QueryPars).

% 返回运行缓慢的 AQL 查询列表
% GET /_api/query/slow
% 查询参数
%    all（可选）：如果设置为true，将返回来自所有数据库的慢查询，而不仅仅是选定的。仅允许在系统数据库中使用该参数并具有超级用户权限。
% 返回一个数组，其中包含在所选数据库中已完成且已超过慢查询阈值的最后 AQL 查询。列表中的最大查询数量可以通过设置查询跟踪属性来控制maxSlowQueries。可以通过设置查询跟踪属性来调整 将查询视为慢速查询的阈值slowQueryThreshold。
% 每个查询都是一个 JSON 对象，具有以下属性：
%    id : 查询的 id
%    database : 运行查询的数据库的名称
%    user : 开始查询的用户名
%    query : 查询字符串（可能被截断）
%    bindVars：查询使用的绑定参数值
%    started：查询开始的日期和时间
%    runTime : 查询的总运行时间
%    state : 查询的当前执行状态（对于慢查询列表将始终为“已完成”）
%    stream : 查询是否使用流式游标
% 返回代码
% 200：查询列表检索成功时返回。
% 400：如果请求格式错误，服务器将响应HTTP 400，
% 403：如果使用了all参数，但请求是在与 _system 不同的数据库中发出的，或者由非特权用户发出的，则返回HTTP 403。
getSlowQuery(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/slow">>).

getSlowQuery(PoolNameOrSocket, QueryPars) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/slow">>, QueryPars).

% 清除慢速AQL查询列表
% DELETE /_api/query/slow
% 清除慢速AQL查询列表
% 返回码
% 200：成功清除查询列表后，服务器将以HTTP 200响应。
% 400：如果请求格式错误，服务器将使用HTTP 400进行响应。
delSlowQuery(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_api/query/slow">>).

delSlowQuery(PoolNameOrSocket, QueryPars) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_api/query/slow">>, QueryPars).

% 杀死查询永久链接
% 运行中的AQL查询也可以在服务器上终止。ArangoDB通过HTTP接口提供了终止功能。要终止正在运行的查询，必须指定其ID（在当前正在运行的查询列表中为该查询返回的ID）。然后将设置查询的kill标志，并且查询到达取消点后将中止查询。

% 杀死一个AQL查询
% DELETE /_api/query/{query-id}
% 路径参数
%     query-id（必填）：查询的ID。
% 终止正在运行的查询。查询将在下一个取消点终止。
% 返回码
%     200：当执行终止请求并设置查询的终止标志时，查询仍在运行时，服务器将以HTTP 200响应。
%     400：如果请求格式错误，服务器将使用HTTP 400进行响应。
%     404：当找不到指定ID的查询时，服务器将以HTTP 404响应。
killQuery(PoolNameOrSocket, QueryId) ->
   Path = <<"/_api/query/", (agMiscUtils:toBinary(QueryId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path).

killQuery(PoolNameOrSocket, QueryId, QueryPars) ->
   Path = <<"/_api/query/", (agMiscUtils:toBinary(QueryId))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, QueryPars).

% AQL查询结果缓存的HTTP接口
% 本节介绍用于控制AQL查询结果缓存的API方法。

% 返回AQL查询结果缓存中存储结果的列表
% GET /_api/query-cache/entries
% 返回一个数组，其中包含当前存储在所选数据库的查询结果缓存中的AQL查询结果。每个结果都是一个具有以下属性的JSON对象：
% hash：查询结果的哈希值
% query：查询字符串
% bindVars：查询的绑定参数。仅当服务器启动时启用了对绑定变量的跟踪时，才显示此属性
% size：查询结果和绑定参数的大小，以字节为单位
% results：查询结果中的文档/行数
% started：查询存储在缓存中的日期和时间
% hits：从缓存中提供结果的次数（对于仅存储在缓存中但以后再也不会访问的查询，可以为 0）
% runTime：查询的运行时间
% dataSources：查询所使用的集合/视图的数组
% 返回码
%    200：可以成功检索结果列表时返回。
%    400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQueryCaches(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query-cache/entries">>).

% 清除AQL查询结果缓存中的所有结果
% DELETE /_api/query-cache
% 清除当前数据库的查询结果缓存
% 返回码
%    200：成功清除缓存后，服务器将以HTTP 200响应。
%    400：如果请求格式错误，服务器将使用HTTP 400进行响应。
clearQueryCaches(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_api/query-cache">>).

% 返回AQL查询结果缓存的全局配置
% GET /_api/query-cache/properties
% 返回全局AQL查询结果缓存配置。该配置是具有以下属性的JSON对象：
%    mode：AQL查询结果缓存运行的模式。该模式是下列值之一：off，on或demand。
%    maxResults：每个特定于数据库的缓存将存储的最大查询结果数。
%    maxResultsSize：每个数据库特定的缓存将存储的查询结果的最大累积大小。
%    maxEntrySize：每个特定于数据库的缓存将存储的查询的最大单个结果大小。
%    includeSystem：是否将涉及系统集合的查询结果存储在查询结果缓存中。
% 返回码
%    200：如果可以成功检索属性，则返回。
%    400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQCacheProps(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query-cache/properties">>).

% 全局调整AQL查询结果缓存属性
% PUT /_api/query-cache/properties
% 具有以下属性的JSON对象是必需的：
%    mode：AQL查询缓存应以哪种模式运行。可能的值是off，on或demand。
%    maxResults：每个特定于数据库的缓存将存储的最大查询结果数。
%    maxResultsSize：每个数据库特定的缓存将存储的查询结果的最大累积大小。
%    maxEntrySize：每个数据库特定的缓存将存储的查询结果的最大单个大小。
%    includeSystem：是否存储涉及系统集合的查询结果。
% 更改属性后，将在HTTP响应中返回当前属性集。
% 注意：更改属性可能会使缓存中的所有结果无效。AQL查询缓存的全局属性。这些属性需要在HTTP请求主体的属性属性中传递。属性必须是具有以下属性的JSON对象：
% 返回码
% 200：如果属性更改成功，则返回。
% 400：如果请求格式错误，服务器将以HTTP 400进行响应，
changeQCacheProps(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/query-cache/properties">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% AQL用户功能管理固定链接
% 这是用于管理AQL用户功能的ArangoDB HTTP接口的简介。AQL用户功能是一种使用用户定义的JavaScript代码扩展ArangoDB查询语言（AQL）功能的方法。
% 有关AQL用户功能及其含义的概述，请参阅“ 扩展AQL”一章。
% HTTP接口提供用于添加，删除和列出以前注册的AQL用户功能的API。
% 通过此接口管理的所有用户功能将存储在系统集合_aqlfunctions中。此集合中的文档不应直接访问，而只能通过专用接口访问。

% 创建一个新的AQL用户功能
% POST /_api/aqlfunction
% 具有以下属性的JSON对象是必需的：
%    name：用户函数的标准名称。
%    code：函数主体的字符串表示形式。
%    isDeterministic：一个可选的布尔值，用于指示函数结果是否完全确定（函数返回值仅取决于输入值，并且对于具有相同输入的重复调用，返回值相同）。该isDeterministic属性是当前未使用但对于优化可以在以后使用。
% 如果成功，则返回HTTP 200。如果该功能无效等，则将返回包含详细错误消息的HTTP 400。
% HTTP 200如果功能已经存在并被调用所取代，则服务器将使用HTTP 200进行响应。
%    error：布尔值标志，指示是否发生错误（在这种情况下为false）
%    code：HTTP状态码
%    isNewlyCreated：布尔值标志，指示是否新创建了函数（在这种情况下为false）
% HTTP 201如果服务器可以注册该功能，则服务器将使用HTTP 201进行响应 。
%    error：布尔值标志，指示是否发生错误（在这种情况下为false）
%    code：HTTP状态码
%    isNewlyCreated：布尔值标志，指示是否新创建了函数（在这种情况下为true）
% HTTP 400如果JSON格式不正确或请求中缺少必需数据，则服务器将使用HTTP 400进行响应。
%    error：布尔值标志，指示是否发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
newUserFun(PoolNameOrSocket, MapData) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/aqlfunction">>, ?AgDefQuery, ?AgDefHeader, eVPack:encode(MapData)).

% 删除现有的AQL用户功能
% DELETE /_api/aqlfunction/{name}
% 路径参数
%     name（必填）：AQL用户功能的名称。
% 查询参数
% group（可选）： - 真：中提供的功能的名称的名称被视为一个命名空间前缀，并在指定的命名空间的所有功能。将被删除。如果没有匹配的字符串，返回的删除函数数可能变为0。
% false：中提供的函数名的名称必须是完全限定，包括任何命名空间。如果没有一个与名称匹配，则返回HTTP 404。

% 删除由name标识的现有AQL用户功能或功能组。
% HTTP 200如果服务器可以删除该功能，则服务器将使用HTTP 200进行响应 。
%     error：布尔值标志，指示是否发生错误（在这种情况下为false）
%     code：HTTP状态码
%     DeleteCount：删除的用户功能的数量，总是1在group设置为false时。设置为true>= 0时的任何数字group
% HTTP 400如果用户功能名称格式错误，则服务器将使用HTTP 400进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
% HTTP 404如果指定的用户用户功能不存在，则服务器将使用HTTP 404进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
delUserFun(PoolNameOrSocket, UserFunName) ->
   Path = <<"/_api/aqlfunction/", (agMiscUtils:toBinary(UserFunName))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path).

delUserFun(PoolNameOrSocket, UserFunName, QueryPars) ->
   Path = <<"/_api/aqlfunction/", (agMiscUtils:toBinary(UserFunName))/binary>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, QueryPars).

% 返回注册的AQL用户功能
% GET /_api/aqlfunction
% 查询参数
%     namespace（可选）：从result下的命名空间namespace返回所有已注册的AQL用户函数。
% 返回所有已注册的AQL用户功能。
% 该调用将返回一个带有状态代码的JSON数组，以及在result下找到的所有用户函数。
% HTTP 200成功HTTP 200返回。
%     error：布尔值标志，指示是否发生错误（在这种情况下为false）
%     code：HTTP状态码
%     result：所有函数，或与命名空间参数匹配的函数
%        name：用户功能的标准名称
%        code：函数体的字符串表示形式
%        isDeterministic：一个可选的布尔值，用于指示函数结果是否完全确定（函数返回值仅取决于输入值，并且对于具有相同输入的重复调用，返回值相同）。该isDeterministic属性是当前未使用但对于优化可以在以后使用。
% HTTP 400如果用户功能名称格式错误，则服务器将使用HTTP 400进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
getUserFuns(PoolNameOrSocket) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/aqlfunction">>).

getUserFuns(PoolNameOrSocket, QueryPars) ->
   agVstCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/aqlfunction">>, QueryPars).