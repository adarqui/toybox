module TB.System.FilePath.Examples (
) where

import           System.FilePath

-- | Some examples
--
-- >>> isAbsolute t_path
-- True
--
-- >>> takeBaseName t_path
-- "services"
--
-- >>> takeExtension t_path
-- ".txt"
--
-- >>> dropExtension t_path
-- "/etc/services"
--
-- >>> replaceExtension t_path "hs"
-- "/etc/services.hs"
--
-- -- >>> t_path -<.> "hs"
-- -- "/etc/services.hs"
--
-- >>> t_path <.> "gz"
-- "/etc/services.txt.gz"
--
-- >>> dropExtensions $ t_path <.> "gz"
-- "/etc/services"
--
-- >>> dropFileName t_path
-- "/etc/"
--
-- >>> dropExtensions t_path `replaceBaseName` "motd"
-- "/etc/motd"
--
-- >>> t_path `replaceDirectory` "/tmp"
-- "/tmp/services.txt"
--
-- >>> "/etc/" `makeRelative` t_path
-- "services.txt"
--
-- >>> normalise "//etc//services"
-- "/etc/services"
--
t_path' = [pathSeparator] </> "etc" </> "services" `addExtension` "txt"
t_path = [pathSeparator] </> "etc" </> "services" <.> "txt"
