
shinylive::export(
  appdir = "myapp",
  destdir = "docs"
)


httpuv::runStaticServer(
  dir = "docs",
  port = 8888
)