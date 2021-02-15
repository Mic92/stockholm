with import <stockholm/lib>;
self: super: {

  bitlbee-facebook = super.bitlbee-facebook.overrideAttrs (old: {
    src = self.fetchFromGitHub {
      owner = "bitlbee";
      repo = "bitlbee-facebook";
      rev = "49ea312d98b0578b9b2c1ff759e2cfa820a41f4d";
      sha256 = "0zg1p9pyfsdbfqac2qmyzcr6zjibwdn2907qgc808gljfx8bfnmk";
    };
  });

  exim = super.exim.overrideAttrs (old: rec {
    version = warnOldVersion old.version "4.95+fixes";
    src = self.fetchgit {
      url = "git://git.exim.org/exim.git";
      rev = "cdb37db5c0ff060de7edfc94e830cab6b7f7c084";
      sha256 = "1xaxs1p8yy5f04an5g9mxhj5cvbnzj0xfb50aa1xxkhkqkspzlsg";
      postFetch = /* sh */ ''
        ${self.gnutar}/bin/tar xf ${old.src}
        ${self.rsync}/bin/rsync -vac "$out"/src/ exim-4.94/src
        rm -R "$out"
        mv exim-4.94 "$out"
      '';
    };
  });

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
