newtype Vector a = Vector { getVector :: [a] } deriving (Eq)
newtype Matrix a = Matrix { getMatrix :: [Vector a] } deriving (Eq)

instance (Eq a, Num a) => Num (Vector a) where
    Vector x + Vector [0] = Vector x
    Vector x + Vector y = Vector (vectorAdd x y)

vectorAdd :: (Num a) => [a] -> [a] -> [a]
vectorAdd _ [] = []
vectorAdd [] _ = []
vectorAdd (x:xs) (y:ys) = x+y:vectorAdd xs ys

instance (Num a, Show a) => Show (Vector a) where
    show (Vector x) = showVector x

instance (Num a, Show a) => Show (Matrix a) where
    show (Matrix x) = showMatrix x

showVector :: (Num a, Show a) => [a] -> String
showVector [x] = show x
showVector (x:xs) = show x ++ "\n" ++ showVector xs

xSpacing :: Int
xSpacing = 10

showMatrix :: (Num a, Show a) => [Vector a] -> String
showMatrix matrix = let formattedMatrix = aZip $ map getVector matrix in showFormattedMatrix formattedMatrix
    where showFormattedMatrix [[]] = ""
          showFormattedMatrix ([]:xs) = "\n" ++ showFormattedMatrix xs
          showFormattedMatrix ((y:ys):xs) = show y ++ (replicate (xSpacing - (length (show y))) ' ') ++ showFormattedMatrix (ys:xs)

matrixFrom2DArray :: (Num a) => [[a]] -> Matrix a
matrixFrom2DArray xs = Matrix (foldr (\x acc -> (Vector x):acc) [] xs)

scalarMult :: (Num a) => Vector a -> a -> Vector a
scalarMult (Vector x) y = Vector (map (*y) x)

matrixTransform :: (Num a, Eq a) => Matrix a -> Vector a -> Vector a
matrixTransform (Matrix []) _ = Vector [0]
matrixTransform _ (Vector []) = Vector [0]
matrixTransform (Matrix (m:ms)) (Vector (v:vs)) = (m `scalarMult` v) + matrixTransform (Matrix (ms)) (Vector (vs))

-- Helper Functions
aZip :: [[a]] -> [[a]]
aZip (x:y:xs) = foldl listZipAppend (listZip x y) xs

listZip :: [a] -> [a] -> [[a]]
listZip [] _ = []
listZip _ [] = []
listZip (x:xs) (y:ys) = [x, y]:listZip xs ys

listZipAppend :: [[a]] -> [a] -> [[a]]
listZipAppend [[]] _ = []
listZipAppend _ [] = []
listZipAppend (x:xs) (y:ys) = (x ++ [y]):listZipAppend xs ys