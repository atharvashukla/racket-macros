#lang racket


(require 2htdp/image)

(overlay (right-triangle 100 100 "solid" "black")
         (rotate 180 (right-triangle 100 100 "solid" "black")))

(square 100 "solid" "black")


