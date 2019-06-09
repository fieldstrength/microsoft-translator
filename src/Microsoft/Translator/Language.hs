{-# LANGUAGE OverloadedStrings #-}

module Microsoft.Translator.Language where

import           Data.Aeson
import           Data.List       (find)
import           Data.Text       (Text)
import           Web.HttpApiData


-- | Languages supported by MS Microsoft.Translator
data Language
    = Afrikaans
    | Arabic
    | Bosnian
    | Bulgarian
    | Catalan
    | ChineseSimplified
    | ChineseTraditional
    | Croatian
    | Czech
    | Danish
    | Dutch
    | English
    | Estonian
    | Finnish
    | French
    | German
    | Greek
    | HaitianCreole
    | Hebrew
    | Hindi
    | HmongDaw
    | Hungarian
    | Indonesian
    | Italian
    | Japanese
    | Kiswahili
    | Klingon
    | KlingonPIqaD
    | Korean
    | Latvian
    | Lithuanian
    | Malay
    | Maltese
    | Norwegian
    | Persian
    | Polish
    | Portuguese
    | QueretaroOtomi
    | Romanian
    | Russian
    | SerbianCyrillic
    | SerbianLatin
    | Slovak
    | Slovenian
    | Spanish
    | Swedish
    | Thai
    | Turkish
    | Ukrainian
    | Urdu
    | Vietnamese
    | Welsh
    | YucatecMaya
    deriving (Show, Eq, Ord, Bounded, Enum)


toLangCode :: Language -> Text
toLangCode Afrikaans          = "af"
toLangCode Arabic             = "ar"
toLangCode Bosnian            = "bs-Latn"
toLangCode Bulgarian          = "bg"
toLangCode Catalan            = "ca"
toLangCode ChineseSimplified  = "zh-CHS"
toLangCode ChineseTraditional = "zh-CHT"
toLangCode Croatian           = "hr"
toLangCode Czech              = "cs"
toLangCode Danish             = "da"
toLangCode Dutch              = "nl"
toLangCode English            = "en"
toLangCode Estonian           = "et"
toLangCode Finnish            = "fi"
toLangCode French             = "fr"
toLangCode German             = "de"
toLangCode Greek              = "el"
toLangCode HaitianCreole      = "ht"
toLangCode Hebrew             = "he"
toLangCode Hindi              = "hi"
toLangCode HmongDaw           = "mww"
toLangCode Hungarian          = "hu"
toLangCode Indonesian         = "id"
toLangCode Italian            = "it"
toLangCode Japanese           = "ja"
toLangCode Kiswahili          = "sw"
toLangCode Klingon            = "tlh"
toLangCode KlingonPIqaD       = "tlh-Qaak"
toLangCode Korean             = "ko"
toLangCode Latvian            = "lv"
toLangCode Lithuanian         = "lt"
toLangCode Malay              = "ms"
toLangCode Maltese            = "mt"
toLangCode Norwegian          = "no"
toLangCode Persian            = "fa"
toLangCode Polish             = "pl"
toLangCode Portuguese         = "pt"
toLangCode QueretaroOtomi     = "otq"
toLangCode Romanian           = "ro"
toLangCode Russian            = "ru"
toLangCode SerbianCyrillic    = "sr-Cyrl"
toLangCode SerbianLatin       = "sr-Latn"
toLangCode Slovak             = "sk"
toLangCode Slovenian          = "sl"
toLangCode Spanish            = "es"
toLangCode Swedish            = "sv"
toLangCode Thai               = "th"
toLangCode Turkish            = "tr"
toLangCode Ukrainian          = "uk"
toLangCode Urdu               = "ur"
toLangCode Vietnamese         = "vi"
toLangCode Welsh              = "cy"
toLangCode YucatecMaya        = "yua"

instance ToHttpApiData Language where
    toUrlPiece = toLangCode

instance FromJSON Language where
    -- FIXME: performance :\
    parseJSON jsonVal
        = maybe (fail $ "Unrecognized language: " ++ show jsonVal) pure
        $ find (\lang -> toJSON (toLangCode lang) == jsonVal) [minBound..]
