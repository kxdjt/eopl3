#lang racket

(require "tests.rkt")
(require (prefix-in gecont- "global-econt/inter.rkt"))
(require (prefix-in gecont-fc- "global-econt/inter-fc.rkt"))
#| (require (prefix-in tra- "trampolined-inter.rkt")) |#
(require (prefix-in 2cont- "pass-two-cont/inter.rkt"))
(require (prefix-in vor- "value-or-resume/inter.rkt"))

