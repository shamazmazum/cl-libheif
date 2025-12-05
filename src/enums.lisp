(in-package :cl-libheif)

(defcenum heif-colorspace
  (:undefined 99)
  (:ycbcr     0)
  :rgb
  :monochrome
  :nonvisual)

(deftype colorspace ()
  "A type for the colorspace of an image."
  '(member :undefined :ycbcr :rgb :monochrome :nonvisual))

(defcenum heif-chroma
  (:undefined 99)
  (:monochrome 0)
  :420
  :422
  :444
  (:interleaved-rgb 10)
  :interleaved-rgba
  :interleaved-rrggbb-be
  :interleaved-rrggbbaa-be
  :interleaved-rrggbb-le
  :interleaved-rrggbbaa-le)

(deftype chroma ()
  "A type which represents how color is encoded in the image."
  '(member
    :undefined :monochrome :420 :422 :444
    :interleaved-rgb
    :interleaved-rgba
    :interleaved-rrggbb-be
    :interleaved-rrggbbaa-be
    :interleaved-rrggbb-le
    :interleaved-rrggbbaa-le))

(defcenum heif-channel
  :y :cb :cr
  :r :g :b
  :alpha
  (:interleaved 10)
  :filter-array
  :depth
  :disparity)

(deftype channel ()
  "A type which represents a channel in the image."
  '(member
    :y :cb :cr
    :r :g :b
    :alpha :interleaved
    :filter-array :depth :disparity))

(defcenum heif-compression-format
  :undefined
  :hevc
  :avc
  :jpeg
  :av1
  :vvc
  :evc
  :jpeg2000
  :uncompressed
  :mask
  :htj2k)

(deftype compression-format ()
  "A type which represents a compression format and is used in encoder
selection."
  '(member
    :undefined
    :hevc
    :avc
    :jpeg
    :av1
    :vvc
    :evc
    :jpeg2000
    :uncompressed
    :mask
    :htj2k))
