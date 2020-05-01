with import <stockholm/lib>;
self: super: {

  flameshot = super.flameshot.overrideAttrs (old: rec {
    patches = old.patches or [] ++ [
      (self.writeText "flameshot-imgur.patch" /* diff */ ''
--- a/src/tools/imgur/imguruploader.cpp
+++ b/src/tools/imgur/imguruploader.cpp
@@ -40,6 +40,7 @@
 #include <QTimer>
 #include <QJsonDocument>
 #include <QJsonObject>
+#include <stdlib.h>
 
 ImgurUploader::ImgurUploader(const QPixmap &capture, QWidget *parent) :
     QWidget(parent), m_pixmap(capture)
@@ -74,7 +75,10 @@ void ImgurUploader::handleReply(QNetworkReply *reply) {
         QJsonObject json = response.object();
         QJsonObject data = json["data"].toObject();
         m_imageURL.setUrl(data["link"].toString());
-        m_deleteImageURL.setUrl(QString("https://imgur.com/delete/%1").arg(
+        char *deleteImageURLPattern = secure_getenv("IMGUR_DELETE_URL");
+        if (deleteImageURLPattern == NULL)
+            deleteImageURLPattern = "https://imgur.com/delete/%1";
+        m_deleteImageURL.setUrl(QString(deleteImageURLPattern).arg(
                                     data["deletehash"].toString()));
         onUploadOk();
     } else {
@@ -105,7 +109,10 @@ void ImgurUploader::upload() {
     QString description = FileNameHandler().parsedPattern();
     urlQuery.addQueryItem("description", description);
 
-    QUrl url("https://api.imgur.com/3/image");
+    char *createImageURLPattern = secure_getenv("IMGUR_CREATE_URL");
+    if (createImageURLPattern == NULL)
+        createImageURLPattern = "https://api.imgur.com/3/image";
+    QUrl url(createImageURLPattern);
     url.setQuery(urlQuery);
     QNetworkRequest request(url);
     request.setHeader(QNetworkRequest::ContentTypeHeader,
      '')
    ];
  });

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';

}
