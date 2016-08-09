-define(API, "/1.0").

-record(http_request, {
          server,
          port,
          opts = [],
          operation,
          resource,
          data = <<>>,
          api_version = nil
         }).

-type http_request() :: #http_request{}.

-record(http_reply, {
          status,
          json = [],
          etag
         }).

-type http_reply() :: #http_reply{}.
