from traderClient import *

c = TraderClient()

c.postOrder(Order(side='Bid',volume=-100,price=15))
print(c.getBookState())

