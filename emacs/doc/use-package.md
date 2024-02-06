use-package gives a few different ways to defer package loading:
- :hook - Package will be loaded the first time one of the hooks is invoked
- :bind - Package will be loaded the first time one of the key bindings is used
- :commands - Package will be loaded when one of the commands are used
- :mode - Package will be loaded the first time a file with a particular extension is opened
- :after - Load this package after other specific packages are loaded
- :defer - If you don’t use any of the other options, this one will defer loading until after startup

If we want to make sure a package gets loaded at startup despite the use of any of the options
above, use :demand t.

```
(with-eval-after-load 'PACKAGE
)
```
