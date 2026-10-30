;;; elfeed-feeds.el --- Feed list for Elfeed -*- lexical-binding: t; -*-

;; One feed per line: ("URL" tag1 tag2 ...).  A bare "URL" also works.
;; Edits are picked up the next time feeds are fetched (G in *elfeed-search*).

(setq elfeed-feeds
      '(;; Uncategorized
        "https://viruta.org/feeds/atom.xml"                                 ; Federico's Blog
        "https://planet.emacslife.com/atom.xml"                             ; Planet Emacslife
        "https://planet.guix.gnu.org/atom.xml"                              ; Planet Guix

        ;; Computation
        ("https://cestlaz.zamansky.net/rss.xml" computation)                ; C'est la Z
        ("https://home.cern/category/computing/feed/atom/" computation)     ; CERN: Computing updates
        ("https://telescoper.blog/feed/atom/" computation)                  ; In the Dark
        ("https://hpc.guix.info/blog/feed.xml" computation)                 ; Guix-HPC
        ("https://julialang.org/feed.xml" computation)                      ; JuliaLang

        ;; Misc
        ("https://home.cern/category/physics/feed/atom/" misc)              ; CERN: Physics updates
        ("https://digofreitas.com/feed/" misc)                              ; Diário de Ideias Gráficas
        ("https://www.itchyfeetcomic.com/feeds/posts/default" misc)         ; Itchy Feet
        ("https://www.lifehack.org/feed/atom" misc)                         ; LifeHack
        ("https://lifehacker.com/rss" misc)                                 ; Lifehacker
        ("https://mentirinhas.com.br/feed/atom/" misc)                      ; Mentirinhas
        ("https://feeds.feedburner.com/naukas" misc)                        ; Naukas
        ("https://feeds.feedburner.com/NoticiasDeLaCienciaYLaTecnologia" misc) ; Noticias de la Ciencia
        ("https://profmattstrassler.com/feed/atom/" misc)                   ; Of Particular Significance
        ("https://feeds.feedburner.com/PapodehomemLifestyleMagazine" misc)  ; Papo de Homem
        ("http://feeds.feedburner.com/blogspot/hNAhW" misc)                 ; Physics and Physicists
        ("https://planet.gentoo.org/rss20.xml" misc)                        ; Planet Gentoo
        ("https://www.quantamagazine.org/feed/atom/" misc)                  ; Quanta Magazine
        ("https://quantumfrontiers.com/feed/atom/" misc)                    ; Quantum Frontiers
        ("https://www.r-bloggers.com/feed/atom/" misc)                      ; R-bloggers
        ("https://marcoarmello.wordpress.com/feed/atom/" misc)              ; Sobrevivendo na Ciência
        ("https://blog.tecosaur.com/tmio/rss.xml" misc)                     ; This Month in Org
        ("https://www.umsabadoqualquer.com/feed/atom/" misc)                ; Um Sábado Qualquer
        ("http://feeds.feedburner.com/vidasuporte" misc)                    ; Vida de Suporte
        ("https://terrytao.wordpress.com/feed/atom/" misc)                  ; What's new
        ("https://developerslife.tech/pt/feed.xml" misc)                    ; Vida de Programador

        ;; Technology
        ("https://www.majorgeeks.com/files/rss" technology)                 ; MajorGeeks.Com
        ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" technology) ; NYT > Technology
        ("http://feeds.hanselman.com/ScottHanselman" technology)            ; Scott Hanselman's Blog
        ("https://rss.slashdot.org/Slashdot/slashdotMainatom" technology)   ; Slashdot
        ("https://www.gentoo.org/feeds/news.xml" technology)                ; Gentoo Linux news
        ("https://guix.gnu.org/feeds/blog.atom" technology)                 ; GNU Guix Blog
        ("https://opensource.googleblog.com/atom.xml" technology)           ; Google Open Source Blog
        ("https://planet.gnome.org//rss20.xml" technology)                  ; Planet GNOME
        ("https://www.theverge.com/rss/index.xml" technology)))             ; The Verge
