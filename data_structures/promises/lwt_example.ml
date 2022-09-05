#require "lwt"
let p, r = Lwt.wait()
let _ = Lwt.wakeup r 42
let _ = Lwt.state p