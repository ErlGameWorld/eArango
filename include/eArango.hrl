-define(AgDelete, 0).
-define(AgGet, 1).
-define(AgPost, 2).
-define(AgPut, 3).
-define(AgHead, 4).              %% (not used in VPP)
-define(AgPatch, 5).
-define(AgOptions, 6).           %% (not used in VPP)

-define(AgDefQuery, #{}).
-define(AgDefHeader, #{}).
-define(AgDefBody, <<>>).