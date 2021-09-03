-module(agBulkImports).
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% IMY-todo 这个模块不可用

% doc_address:https://www.arangodb.com/docs/stable/http/bulk-imports.html

% 批量导入的HTTP接口
% ArangoDB提供了一个HTTP接口，可以一次将多个文档导入一个集合中。这称为批量导入。
% 上传的数据必须以JSON格式提供。有两种导入数据的机制：
% 自包含的JSON文档：在这种情况下，每个文档都包含所有属性名称和值。在上传的文档中，属性名称可能完全不同
% 属性名称加上文档数据：在这种情况下，第一个数组必须包含后面文档的属性名称。以下数组仅包含属性值。属性值将按位置映射到属性名称。
% 两种输入机制的端点地址均为/ _api / import。必须使用HTTP POST请求将数据发送到此URL。要导入的数据必须包含在POST请求的正文中。
% 该集合的查询参数必须用于指定目标集合导入。将数据导入到不存在的集合中将产生错误。
% 可以将waitForSync查询参数设置为true，以仅在所有文档都已同步到磁盘后才返回导入。
% 如果任何上传的文档无效并且无法导入，可以将complete查询参数设置为true，以使整个导入失败。在这种情况下，即使在导入结束时发生故障，导入操作也不会导入任何文档。
% 如果complete具有除true以外的其他值，则将导入有效文档，而拒绝无效文档，这意味着可能仅导入了一些上载文档。
% 可以将details查询参数设置为true，以使导入API返回有关无法导入的文档的详细信息。如果details为true，则结果还将包含一个details属性，该属性是详细错误消息的数组。如果将详细信息设置为false或省略，则不会返回任何详细信息。

% 从JSON编码的列表中导入文档
% POST /_api/import#document
% 查询参数
%    collection （必填）：集合名称。
%    fromPrefix（可选）：_from属性中值的可选前缀。如果指定，该值将自动添加到每个_from输入值之前。这样就可以仅指定的键_from。
%    toPrefix（可选）：_to属性中值的可选前缀。如果指定，该值将自动添加到每个_to输入值之前。这样就可以仅指定的键_to。
%    overwrite （可选）：如果此参数的值为true或yes，则将在导入之前删除集合中的所有数据。请注意，任何现有的索引定义都将保留。
%    waitForSync（可选）：等待文档同步到磁盘后再返回。
%    onDuplicate（可选）：控制在违反唯一键约束的情况下执行的操作。可能的值为：
%       error：由于违反唯一键约束，因此不会导入当前文档。这是默认设置。
%       update：这将使用请求中指定的数据更新数据库中的现有文档。请求中不存在的现有文档的属性将被保留。
%       replace：这将用请求中指定的数据替换数据库中的现有文档。
%       ignore：这不会更新现有文档，而只是忽略由唯一键约束冲突引起的错误。
% 请注意，仅当请求中的导入文档包含_key属性时，update，replace和ignore才起作用。由于次要唯一键约束冲突，更新和 替换也可能失败。
%
%    complete （可选）：如果设置为true或yes，则如果发生任何错误，将使整个导入失败。否则，即使无法导入某些文档，导入也将继续。
%    details（可选）：如果设置为true或yes，结果将包括一个属性，details 其中包含有关无法导入的文档的详细信息。
%
% 请求正文（字符串）
% 主体必须由JSON编码的属性值数组组成，每个文档一行。请求的第一行必须是JSON编码的属性名称数组。这些属性名称用于后续各行中的数据。
% 在由标识的集合中创建文档collection-name。请求正文的第一行必须包含一个JSON编码的属性名称数组。请求正文中的以下所有行都必须包含JSON编码的属性值数组。每行都被解释为一个单独的文档，并且指定的值将映射到在第一标题行中指定的属性名称的数组。
%
% 响应是具有以下属性的JSON对象：
%    created：导入的文件数。
%    errors：由于错误而未导入的文档数。
%    empty：在输入中找到的空行数（类型documents或只能包含大于零的值auto）。
%    updated：更新/替换的文档数（如果onDuplicate 设置为update或replace）。
%    ignored：失败但被忽略的插入操作数（如果 onDuplicate设置为ignore）。
%    details：如果查询参数details设置为true，则结果将包含一个details属性，该属性是一个数组，其中包含有关无法插入哪些文档的更多详细信息。
%
% 返回码
%    201：如果可以成功导入所有文档，则返回。
%    400：如果type包含无效值，未collection指定no ，文档编码错误或请求格式错误，则返回。
%    404：如果collection或导入边的_from或_to属性引用未知集合，则返回。
%    409：如果导入会触发唯一键冲突，complete则返回，并将 其设置为true。
%    500：如果服务器无法为没有用户定义密钥的文档自动生成文档密钥（密钥错误），则返回500。
docImports(PoolNameOrSocket, ListOfList, QueryPars) ->
   BodyStr = <<<<(eVPack:encodeBin(OneList))/binary, "\n">> || OneList <- ListOfList>>,
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/import">>, QueryPars, ?AgDefHeader, BodyStr).

% 从JSON导入文档
% POST /_api/import#json
% 查询参数
%    type （必填）：确定如何解释请求的正文。type可以具有以下值：
%       documents：使用此类型时，请求正文中的每一行都应为单独的JSON编码的文档。请求主体中的多个JSON对象需要用换行符分隔。
%       list：使用此类型时，请求主体必须包含要导入的单个对象的单个JSON编码数组。
%       auto：如果设置，它将自动确定主体类型（ documents或list）。
%    collection （必填）：集合名称。
%    fromPrefix（可选）：_from属性中值的可选前缀。如果指定，该值将自动添加到每个_from输入值之前。这样就可以仅指定的键_from。
%    toPrefix（可选）：_to属性中值的可选前缀。如果指定，该值将自动添加到每个_to输入值之前。这样就可以仅指定的键_to。
%    overwrite （可选）：如果此参数的值为true或yes，则将在导入之前删除集合中的所有数据。请注意，任何现有的索引定义都将保留。
%    waitForSync（可选）：等待文档同步到磁盘后再返回。
%    onDuplicate（可选）：控制在违反唯一键约束的情况下执行的操作。可能的值为：
%       error：由于违反唯一键约束，因此不会导入当前文档。这是默认设置。
%       update：这将使用请求中指定的数据更新数据库中的现有文档。请求中不存在的现有文档的属性将被保留。
%       replace：这将用请求中指定的数据替换数据库中的现有文档。
%       ignore：这不会更新现有文档，而只是忽略由唯一键约束冲突引起的错误。
%    请注意，仅当请求中的导入文档包含_key属性时，update，replace和ignore才起作用。由于次要唯一键约束冲突，更新和 替换也可能失败。
%    complete （可选）：如果设置为true或yes，则如果发生任何错误，将使整个导入失败。否则，即使无法导入某些文档，导入也将继续。
%    details（可选）：如果设置为true或yes，结果将包括一个属性，details 其中包含有关无法导入的文档的详细信息。
% 请求正文（字符串）
% 主体必须是JSON编码的对象数组，或者是包含多个以换行符分隔的JSON对象的字符串。
% 在由标识的集合中创建文档collection-name。文档的JSON表示形式必须作为POST请求的主体传递。请求主体可以由多行组成，每行都是一个独立的JSON对象，也可以是包含子对象的JSON数组。
%
% 响应是具有以下属性的JSON对象：
%    created：导入的文件数。
%    errors：由于错误而未导入的文档数。
%    empty：在输入中找到的空行数（类型documents或只能包含大于零的值auto）。
%    updated：更新/替换的文档数（如果onDuplicate 设置为update或replace）。
%    ignored：失败但被忽略的插入操作数（如果 onDuplicate设置为ignore）。
%    details：如果查询参数details设置为true，则结果将包含一个details属性，该属性是一个数组，其中包含有关无法插入哪些文档的更多详细信息。
% 返回码
%    201：如果可以成功导入所有文档，则返回。
%    400：如果type包含无效值，未collection指定no ，文档编码错误或请求格式错误，则返回。
%    404：如果collection或导入边的_from或_to属性引用未知集合，则返回。
%    409：如果导入会触发唯一键冲突，complete则返回，并将 其设置为true。
%    500：如果服务器无法为没有用户定义密钥的文档自动生成文档密钥（密钥错误），则返回500。
jsonImports(PoolNameOrSocket, MapDataList, QueryPars) ->
   case QueryPars of
      #{type := list} ->
         BodyStr = eVPack:encodeBin(MapDataList);
      #{type := documents} ->
         BodyStr = <<<<(eVPack:encodeBin(OneList))/binary, "\n">> || OneList <- MapDataList>>;
      _ ->
         BodyStr = MapDataList
   end,
   agVstCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/import">>, QueryPars, ?AgDefHeader, BodyStr).