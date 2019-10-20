dev:
	@ghcid --command="stack ghci --ghci-options='+RTS -M2G -RTS -fwarn-unused-binds -fwarn-unused-imports -isrc :load Main'" -T :main

test-app:
	@ghcid --command="stack ghci --ghci-options='+RTS -M2G -RTS -fwarn-unused-binds -fwarn-unused-imports -isrc' test/Spec.hs" -T :main
