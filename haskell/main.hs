import Data.List (intercalate)
import Data.Time (getCurrentTime, utctDay, diffDays)
import Data.Time.Calendar (fromGregorian, toGregorian)

pat 'S' = lines "#### \n#    \n#    \n ### \n    #\n    #\n#### "
pat 'H' = lines "#   #\n#   #\n#   #\n#####\n#   #\n#   #\n#   #"
pat 'O' = lines " ### \n#   #\n#   #\n#   #\n#   #\n#   #\n ### "
pat 'E' = lines "#####\n#    \n#    \n#### \n#    \n#    \n#####"
pat 'I' = lines " ### \n  #  \n  #  \n  #  \n  #  \n  #  \n ### "
pat 'A' = lines " ### \n#   #\n#   #\n#####\n#   #\n#   #\n#   #"
pat  _  = replicate 7 "     "

render s = unlines [intercalate "   " [g!!i | g <- map pat s] | i <- [0..6]]

center w s = let l = sum [if c <= '\x7f' then 1 else 2 | c <- s]
            in replicate ((w - l) `div` 2) ' ' ++ s

main :: IO ()
main = do
  putStrLn (render "SHOEISHA")
  t <- utctDay <$> getCurrentTime
  let (y, m, d) = toGregorian t
      n = if t >= fromGregorian y 12 19 then y - 1984 else y - 1985
      days = diffDays t (fromGregorian 1985 12 19) + 1
  putStrLn $ "創業" ++ show n ++ "年目 / " ++ show days ++ "日目"
  case (m, d, days `mod` 1000, days `mod` 365) of
    (12, 19, _, _) -> putStrLn "🎉 祝！創業記念日 🎉"
    (_, _, 0, _)   -> putStrLn $ "✨ 祝 " ++ show days ++ " 日 ✨"
    (_, _, _, 0)   -> putStrLn $ "🎊 祝 " ++ show (days `div` 365) ++ " 年 🎊"
    _              -> pure ()