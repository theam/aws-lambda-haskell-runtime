---
title: Common errors
---

# Error while loading shared libraries

Given the native nature of Haskell, and it's possibility to interoperate with shared libraries,
it might be possible that you have encountered yourself facing an error that looks like:

```text
/var/task/bootstrap: error while loading shared libraries: libpcre.so.3: cannot open shared object file: No such file or directory
```

## Solution

To solve this, we have to make sure that whatever is packaging/deploying our function is copying these libraries to our function package. If you are using the scaffold example provided by The Agile Monkeys, then you should substitute the last line of your makefile for these lines:
`
@rsync -rl <path-to-your-extra-libraries>
@cd build && zip -r function.zip bootstrap <name-of-library-directory> && rm bootstrap && cd ..
`
additionally, you will want to make sure you have the amd64 version of these libraries, and when you download them that they live in lib/x86_64/. When you download amd64 files they are usually saved to lib/x86_64_gnu_linux.
Please note that you might have to adjust your `LD_LIBRARY_PATH` environment variable, so it also points to the directory where you've
placed these files.

Here is a more comprehensive list of libraries that you might want to copy if they are used by your project. You can use it to check if
some library of those might appear in your project.

```text
libz.so.1
libxml2.so.2
libverto.so.1
libutil.so.1
libunistring.so.0
libtinfo.so.5
libtic.so.5
libthread_db.so.1
libtasn1.so.3
libstdc++.so.6
libssl3.so
libssl.so.10
libssh2.so.1
libsqlite3.so.0
libsoftokn3.so
libsmime3.so
libsepol.so.1
libselinux.so.1
libsasl2.so.2
librt.so.1
librpmsign.so.1
librpmio.so.3
librpmbuild.so.3
librpm.so.3
libresolv.so.2
libreadline.so.6
libp11-kit.so.0
libpython2.7.so.1.0
libpthread.so.0
libpth.so.20
libpsl.so.0
libpopt.so.0
libplds4.so
libplc4.so
libpcreposix.so.0
libpcrecpp.so.0
libpcre.so.0
libpcprofile.so
libpanelw.so.5
libpanel.so.5
libnss3.so
libnssutil3.so
libnsssysinit.so
libnsspem.so
libnssdbm3.so
libnssckbi.so
libnss_nisplus.so.2
libnss_nis.so.2
libnss_hesiod.so.2
libnss_files.so.2
libnss_dns.so.2
libnss_db.so.2
libnss_compat.so.2
libnspr4.so
libnsl.so.1
libncursesw.so.5
libncurses.so.5
libmenuw.so.5
libmenu.so.5
libmemusage.so
libmagic.so.1
libm.so.6
liblzma.so.5
liblua-5.1.so
libldif-2.4.so.2
libldap_r-2.4.so.2
libldap-2.4.so.2
liblber-2.4.so.2
libk5crypto.so.3
libkrb5support.so.0
libkrb5.so.3
libkrad.so.0
libkeyutils.so.1
libkdb5.so.8
libidn2.so.0
libicuuc.so.50
libicutu.so.50
libicutest.so.50
libiculx.so.50
libicule.so.50
libicui18n.so.50
libicuio.so.50
libicudata.so.50
libhistory.so.6
libgthread-2.0.so.0
libgssrpc.so.4
libgssapi_krb5.so.2
libgpgme.so.11
libgpgme-pthread.so.11
libgpg-error.so.0
libgobject-2.0.so.0
libgmpxx.so.4
libgmp.so.10
libgmodule-2.0.so.0
libglib-2.0.so.0
libgio-2.0.so.0
libgdbm.so.2
libgcrypt.so.11
libgcc_s.so.1
libfreebl3.so
libfreeblpriv3.so
libformw.so.5
libform.so.5
libffi.so.6
libexpat.so.1
libelf.so.1
libdl.so.2
libdb-4.7.so
libcurl.so.4
libcrypto.so.10
libcrypt.so.1
libcom_err.so.2
libcidn.so.1
libcap.so.2
libc.so.6
libbz2.so.1
libattr.so.1
libassuan.so.0
libanl.so.1
libacl.so.1
libSegFault.so
libBrokenLocale.so.1
ld-linux-x86-64.so.2
```
