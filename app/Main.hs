import           Control.Exception    (SomeException, try)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import           Data.Either          (lefts, rights)
import           Data.List            (isInfixOf)
import           Data.List.Split      (splitOn)
import qualified Data.Map             as M
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Sum (..), getSum)
import           Data.Time            (defaultTimeLocale, parseTimeM)
import           Data.Time.Calendar   (Day, fromGregorian)
import           GHC.Generics         (Generic)

-- Error handling
data AppError
  = FileError String
  | ParseError String
  deriving (Show, Eq)

instance Semigroup AppError where
  FileError e1 <> FileError e2   = FileError (e1 <> "; " <> e2)
  ParseError e1 <> ParseError e2 = ParseError (e1 <> "; " <> e2)
  FileError e <> _               = FileError e
  _ <> FileError e               = FileError e

instance Monoid AppError where
  mempty = FileError ""

-- Priority Data Type
data Priority
  = High
  | Medium
  | Low
  deriving (Eq, Ord, Show)

-- Expense Data
data Expense = Expense
  { expenseId       :: String
  , expenseName     :: String
  , expenseAmount   :: Double
  , expenseCategory :: String
  , expenseDate     :: Day
  } deriving (Show, Generic)

instance ToJSON Expense

instance FromJSON Expense

-- Income Data
data Income = Income
  { incomeId     :: String
  , incomeSource :: String
  , incomeAmount :: Double
  , incomeDate   :: Day
  } deriving (Show, Generic)

instance FromJSON Income

instance ToJSON Income

-- Budget data type
data Budget = Budget
  { budgetCategory :: String
  , budgetAmount   :: Double
  } deriving (Show, Generic)

instance ToJSON Budget

instance FromJSON Budget

-- History Data
data History = History
  { historyDate   :: Day
  , totalExpenses :: Sum Double
  , totalIncomes  :: Sum Double
  } deriving (Show)

instance Semigroup History where
  History date1 exp1 inc1 <> History _ exp2 inc2 =
    History date1 (exp1 <> exp2) (inc1 <> inc2)

instance Monoid History where
  mempty = History (fromGregorian 1970 1 1) (Sum 0) (Sum 0)

-- Error handling and file operations
loadFromFile :: FromJSON a => FilePath -> IO (Either AppError [a])
loadFromFile path = do
  result <- try (B.readFile path) :: IO (Either SomeException B.ByteString)
  return
    $ case result of
        Left err      -> Left (FileError (show err))
        Right content -> either (Left . ParseError) Right (eitherDecode content)

saveToFile :: ToJSON a => FilePath -> [a] -> IO ()
saveToFile path = B.writeFile path . encode

-- Generate new IDs
generateNewId :: [a] -> String -> String
generateNewId list inf = inf ++ show (length list + 1)

-- Parsing
parseExpense :: String -> Either String Expense
parseExpense str =
  case splitOn [','] str of
    [name, amount, category, dateStr] ->
      case stringToDay dateStr of
        Just date ->
          Right
            Expense
              { expenseId = ""
              , expenseName = name
              , expenseAmount = read amount
              , expenseCategory = category
              , expenseDate = date
              }
        Nothing -> Left "Invalid date format"
    _ -> Left "Invalid expense format"

-- Add expenses and incomes
addExpenseToFile :: Expense -> IO (Either AppError ())
addExpenseToFile expense = do
  result <- loadFromFile "expenses.json"
  case result of
    Left err -> return $ Left err
    Right exps -> do
      let updatedExpenses = expense {expenseId = generateNewId exps "E"} : exps
      saveToFile "expenses.json" updatedExpenses
      return $ Right ()

-- Handle Results
handleResults :: [Either AppError ()] -> IO ()
handleResults results = do
  let allErrors = mconcat (lefts results) -- Combine all errors into one
  if allErrors == mempty
    then putStrLn "All operations succeeded!"
    else putStrLn $ "Errors occurred: " <> show allErrors

-- Add Multiple Expenses and Incomes
addMultipleExpenses :: [Expense] -> IO ()
addMultipleExpenses expenses = do
  results <- mapM addExpenseToFile expenses
  handleResults results

-- Collect lines until an empty line is entered
collectLines :: IO [String]
collectLines = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- collectLines
      return (line : rest)

------------------------------------------------------------
-- Chuyển String thành Day
stringToDay :: String -> Maybe Day
stringToDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Hàm parseIncome để chuyển từ String thành Income
parseIncome :: String -> Either String Income
parseIncome str =
  case splitOn "," str of
    [source, amount, dateStr] ->
      case stringToDay dateStr of
        Just date ->
          Right
            Income
              { incomeId = ""
              , incomeSource = source
              , incomeAmount = read amount
              , incomeDate = date
              }
        Nothing -> Left "Invalid date format"
    _ -> Left "Invalid income format"

addIncomeToFile :: Income -> IO (Either AppError ())
addIncomeToFile income = do
  result <- loadFromFile "incomes.json"
  case result of
    Left err -> return $ Left err
    Right incs -> do
      let updatedIncomes = income {incomeId = generateNewId incs "I"} : incs
      saveToFile "incomes.json" updatedIncomes
      return $ Right ()

addMultipleIncomes :: [Income] -> IO ()
addMultipleIncomes incomes = do
  results <- mapM addIncomeToFile incomes
  handleResults results

-- expenses1 :: [Expense]
-- expenses1 =
--   [ Expense "E1" "Rent" 500 "Housing" "2024-08-01"
--   , Expense "E2" "Utilities" 100 "Bills" "2024-08-01"
--   ]
-- ex1 = Expense "E2" "Utilities" 100 "Bills" "2024-08-01"
-- ex2 = Expense "E2" "Utilities" 100 "Bills" "2024-08-01"
-- -- ex3 = ex1 <> ex2
-- incomes1 :: [Income]
-- incomes1 = [Income "I1" "Salary" 1000 "2024-08-01"]
-- test1 = generateHistory expenses1 incomes1
-- test2 = M.toList test1
-------------------------------------------------------------------------------------------
generateHistory :: [Expense] -> [Income] -> M.Map Day History
generateHistory expenses incomes =
  let expenseHistory =
        map
          (\e ->
             ( expenseDate e
             , History (expenseDate e) (Sum $ expenseAmount e) mempty))
          expenses
      incomeHistory =
        map
          (\i ->
             ( incomeDate i
             , History (incomeDate i) mempty (Sum $ incomeAmount i)))
          incomes
      combined =
        M.unionWith
          (<>)
          (M.fromListWith (<>) expenseHistory)
          (M.fromListWith (<>) incomeHistory)
   in combined

-- Print Financial History
printHistory :: FilePath -> FilePath -> IO ()
printHistory expensesPath incomesPath = do
  expensesResult <- loadFromFile expensesPath
  incomesResult <- loadFromFile incomesPath
  case (expensesResult, incomesResult) of
    (Right expenses, Right incomes) -> do
      let history = generateHistory expenses incomes
      putStrLn "Financial History:"
      mapM_
        (\(date, report) ->
           putStrLn
             $ show date
                 ++ ": Expenses: "
                 ++ show (getSum $ totalExpenses report)
                 ++ ", Incomes: "
                 ++ show (getSum $ totalIncomes report))
        (M.toList history)
    (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
    (_, Left err) -> putStrLn $ "Error loading incomes: " ++ show err

----------------------------------------------------------------------------------
-- Function to compute expenses by priority using Monoid
determinePriority :: String -> Priority
determinePriority category
  | category `elem` ["Rent", "Food", "Medical"] = High
  | category `elem` ["Entertainment", "Travel", "Shopping"] = Medium
  | otherwise = Low

expensesByPriority :: [Expense] -> M.Map Priority Double
expensesByPriority = M.map getSum . foldr updatePriority M.empty
  where
    updatePriority expense acc =
      let priority = determinePriority (expenseCategory expense) -- Determine priority based on category
          amount = Sum $ expenseAmount expense
       in M.insertWith (<>) priority amount acc

printExpensesByPriority :: FilePath -> IO ()
printExpensesByPriority path = do
  result <- loadFromFile path
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right expenses -> do
      let priorityMap = expensesByPriority expenses
      putStrLn "Expenses by Priority:"
      mapM_
        (\(priority, amt) -> putStrLn $ show priority ++ ": " ++ show amt)
        (M.toList priorityMap)

------------------------------------------------------------------------------------------------------
-- Print Expenses by Category
-- expensesByCategory :: [Expense] -> M.Map String Double
-- expensesByCategory =
--   foldr (M.insertWith (+) <$> expenseCategory <*> expenseAmount) M.empty
expensesByCategory :: [Expense] -> M.Map String Double
expensesByCategory = M.map getSum . foldr updateCategory M.empty
  where
    updateCategory expense acc =
      let category = expenseCategory expense
          amount = Sum $ expenseAmount expense
       in M.insertWith (<>) category amount acc

printExpensesByCategory :: FilePath -> IO ()
printExpensesByCategory path = do
  result <- loadFromFile path
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right expenses -> do
      let categoryMap = expensesByCategory expenses
      putStrLn "Expenses by Category:"
      mapM_
        (\(cat, amt) -> putStrLn $ cat ++ ": " ++ show amt)
        (M.toList categoryMap)

--------------------------------------------------------------
-- Function to compute incomes by source using Monoid
incomesBySource :: [Income] -> M.Map String Double
incomesBySource = M.map getSum . foldr updateSource M.empty
  where
    updateSource income acc =
      let source = incomeSource income
          amount = Sum $ incomeAmount income
       in M.insertWith (<>) source amount acc

printIncomesBySource :: FilePath -> IO ()
printIncomesBySource path = do
  result <- loadFromFile path
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right incomes -> do
      let sourceMap = incomesBySource incomes
      putStrLn "Incomes by Source:"
      mapM_
        (\(src, amt) -> putStrLn $ src ++ ": " ++ show amt)
        (M.toList sourceMap)

---------------------------------------------------------------------------------------------
-- Search Expenses and Incomes
searchExpenses :: String -> IO ()
searchExpenses query = do
  result <- loadFromFile "expenses.json"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right expenses -> do
      let filtered =
            filter
              (\e ->
                 isInfixOf query (expenseName e)
                   || query `isInfixOf` expenseCategory e)
              expenses
      putStrLn "Search Results for Expenses:"
      mapM_
        (\e -> putStrLn $ expenseName e ++ ": " ++ show (expenseAmount e))
        filtered

--------------------------------------------------------
searchIncomes :: String -> IO ()
searchIncomes query = do
  result <- loadFromFile "incomes.json"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right incomes -> do
      let filtered = filter (\i -> query `isInfixOf` incomeSource i) incomes
      putStrLn "Search Results for Incomes:"
      mapM_
        (\i -> putStrLn $ incomeSource i ++ ": " ++ show (incomeAmount i))
        filtered

-------------------------------------------------------------------------
-- Check if expenses exceed the budget
checkBudgets :: [Expense] -> [Budget] -> M.Map String String
checkBudgets expenses budgets =
  let totalExpensesByCategory = expensesByCategory expenses
      budgetMap =
        M.fromList $ map (\b -> (budgetCategory b, budgetAmount b)) budgets
      check (category, amount) =
        case M.lookup category budgetMap of
          Just budget
            | amount > budget ->
              Just
                ( category
                , category ++ " exceeds budget by " ++ show (amount - budget))
          _ ->
            case M.lookup "Utilities" budgetMap of
              Just budget
                | amount > budget ->
                  Just
                    ( category
                    , category
                        ++ " exceeds budget by "
                        ++ show (amount - budget))
              _ -> Nothing
   in M.fromList $ mapMaybe check (M.toList totalExpensesByCategory)

-------------------------------------------------------------------------------
checkedBudgets :: FilePath -> FilePath -> IO ()
checkedBudgets expensesPath budgetsPath = do
  expensesResult <- loadFromFile expensesPath
  budgetsResult <- loadFromFile budgetsPath -- You need to have this file with Budget data
  case (expensesResult, budgetsResult) of
    (Right expenses, Right budgets) -> do
      let budgetCheck = checkBudgets expenses budgets
      if M.null budgetCheck
        then putStrLn "All expenses are within the budget!"
        else mapM_ (putStrLn . snd) (M.toList budgetCheck)
    (Left err, _) -> putStrLn $ "Error loading expenses: " ++ show err
    (_, Left err) -> putStrLn $ "Error loading budgets: " ++ show err

--------------------------------------------------------------------------------
main :: IO ()
main = mainMenu

-- Main Menu
mainMenu :: IO ()
mainMenu = do
  putStrLn "===== Personal Finance Management ====="
  putStrLn "1. Add Expenses"
  putStrLn "2. Add Incomes"
  putStrLn "3. Display Financial History"
  putStrLn "4. Display Expenses by Priority"
  putStrLn "5. Display Expenses by Category"
  putStrLn "6. Display Incomes by Source"
  putStrLn "7. Search Expenses"
  putStrLn "8. Search Incomes"
  putStrLn "9. Checking budgets:"
  putStrLn "10. Exits"
  putStrLn "Choose an option:"
  option <- getLine
  case option of
    "1" -> do
      putStrLn
        "Enter Expense Details (Name,Amount,Category,Date) or an empty line to finish:"
      expenseLines <- collectLines
      let expenses = rights $ map parseExpense expenseLines
      addMultipleExpenses expenses
      mainMenu
    "2" -> do
      putStrLn
        "Enter Income Details (Source,Amount,Date) or an empty line to finish:"
      incomeLines <- collectLines
      let incomes = rights $ map parseIncome incomeLines
      addMultipleIncomes incomes
      mainMenu
    "3" -> do
      putStrLn "Displaying financial history:"
      printHistory "expenses.json" "incomes.json"
      mainMenu
    "4" -> do
      putStrLn "Displaying expenses by priority:"
      printExpensesByPriority "expenses.json"
      mainMenu
    "5" -> do
      putStrLn "Displaying expenses by category:"
      printExpensesByCategory "expenses.json"
      mainMenu
    "6" -> do
      putStrLn "Displaying incomes by source:"
      printIncomesBySource "incomes.json"
      mainMenu
    "7" -> do
      putStrLn "Enter search query for expenses:"
      query <- getLine
      searchExpenses query
      mainMenu
    "8" -> do
      putStrLn "Enter search query for incomes:"
      query <- getLine
      searchIncomes query
      mainMenu
    "9" -> do
      putStrLn "Checking budgets:"
      checkedBudgets "expenses.json" "budgets.json"
      mainMenu
    "10" -> do
      putStrLn "Exiting..." >> return ()
    _ -> do
      putStrLn "Invalid option. Please choose again."
