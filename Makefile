.PHONY: all

all: dist/SuffixTablify dictionaries/dutch-scrabble dictionaries/dutch-scrabble-suffixes

dictionaries/dutch-scrabble: dictionaries/dutch-derived dictionaries/dutch-valid
	cat $^ | egrep -v '[-A-Z0-9\ \.]' | egrep '..' | tr -d \' | tr 'àáâäçèéêëíîïñóôöúûü' 'aaaaceeeeiiinooouuu' | sort > $@

dist/SuffixTablify: Scrabblatan/SuffixTablify.hs
	stack ghc -- $^ -o $@
	rm $(basename $^).hi $(basename $^).o

dictionaries/%-suffixes: dist/SuffixTablify dictionaries/%
	cat $(filter-out $<,$^) | ./$< | sort -s -k 1,1 > $@
