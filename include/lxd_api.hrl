-record(http_request, {
          server,
          port,
          operation,
          resource,
          data = <<>>,
          opts = []
         }).

-type http_request() :: #http_request{}.

-record(http_reply, {
          status,
          json,
          etag
         }).

-type http_reply() :: #http_reply{}.
