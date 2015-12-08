## 4chan2txt

Downloads text data from 4chan with Haskell.

 * Install [stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md)
 * `git clone https://github.com/cirquit/4chan2txt`
 * `stack build`
 * `stack exec -- 4chan2txt -board g -to g_content.txt`

After the installing stack:

 * `cd 4chan2txt`
 * `stack install`
 * 4chan2txt lies now in `~/.local/bin/`

Info:

 * Downloads 10 pages from the board, splits the responses via `\n` and adds everything to the .txt-file
 * filtering numbers longer than 6 digets