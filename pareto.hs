-- 定义一个类型来表示分配状态
type Allocation = [Int]

-- 检查两个分配状态是否具有帕累托改进
paretoImprove :: Allocation -> Allocation -> Bool
paretoImprove alloc1 alloc2 = all (>= 0) diffs && any (> 0) diffs
  where
    diffs = zipWith (-) alloc2 alloc1

-- 检查给定分配状态是否为帕累托最优
isParetoOptimal :: [Allocation] -> Allocation -> Bool
isParetoOptimal allocations alloc = not $ any (paretoImprove alloc) allocations

-- 示例使用
main :: IO ()
main = do
  let allocations = [[3, 3], [2, 4], [4, 2], [5, 1], [1, 5]]
      allocationToCheck = [3, 3]
  if isParetoOptimal allocations allocationToCheck
    then putStrLn "The allocation is Pareto optimal."
    else putStrLn "The allocation is not Pareto optimal."
