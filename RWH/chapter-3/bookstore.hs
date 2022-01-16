data BookInfo = Book Int String [String] deriving (Show)
data MagazineInfo = Magazine Int String [String] deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

myInfo = Book 123456 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- algebraic data types : types with more than one value constructor
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- pattern matching
bookID (Book idBook title authors) = idBook
bookTitle (Book idBook title authors) = title
bookAuthors (Book idBook title authors) = authors

nicerID (Book idBook _ _) = idBook
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
    } deriving (Show)

customer1 = Customer 1234 "Name 1" ["Street1", "City1", "Country2"]
customer2 = Customer {
    customerID = 567,
    customerAddress = ["street2","city2", "country2"],
    customerName = "Name 2"
    }