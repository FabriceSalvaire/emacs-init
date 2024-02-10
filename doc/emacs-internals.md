# Get help on...

- `describe-char` for the cursor
- `describe-face`
- `describe-function` cursor on a function
- `describe-mode` for a buffer 
- `describe-variable` cursor on a variable
- `describe-`

# Build

`C-h v system-configuration-options`

"--build=x86_64-redhat-linux-gnu --host=x86_64-redhat-linux-gnu --program-prefix= --disable-dependency-tracking --prefix=/usr --exec-prefix=/usr --bindir=/usr/bin --sbindir=/usr/sbin --sysconfdir=/etc --datadir=/usr/share --includedir=/usr/include --libdir=/usr/lib64 --libexecdir=/usr/libexec --localstatedir=/var --runstatedir=/run --sharedstatedir=/var/lib --mandir=/usr/share/man --infodir=/usr/share/info --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-xpm --with-x-toolkit=gtk3 --with-gpm=no --with-xwidgets --with-modules --with-harfbuzz --with-cairo --with-json --with-native-compilation=aot --with-tree-sitter --with-sqlite3 --with-webp --with-xinput2 build_alias=x86_64-redhat-linux-gnu host_alias=x86_64-redhat-linux-gnu CC=gcc 'CFLAGS=-DMAIL_USE_LOCKF -O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1  -m64   -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer ' LDFLAGS=-Wl,-z,relro PKG_CONFIG_PATH=:/usr/lib64/pkgconfig:/usr/share/pkgconfig CXX=g++ 'CXXFLAGS=-O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1  -m64   -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer'"

Pure GTK
`pixel-scroll-precision-mode` is available
    
# Hook

Problems of using lambda in hook:
- Lambda in hook is unreadable when reading value of a hook, such as in `describe-variable` or any keybinding help or log.
- Lambda in hook cannot be removed using `remove-hook`.

