import logging
import requests
from pydantic import BaseModel
import pandas as pd

class Order(BaseModel):
    side: str
    volume: int
    price: int

class TraderClient:
    def __init__(self, url='http://127.0.0.1:8000') -> None:
        self.url = url
        self.logLevel = logging.INFO
        self.setLogLevel(self.logLevel)

    def setLogLevel(self, logLevel) -> None:
        self.logLevel = logLevel
        logging.basicConfig(level=self.logLevel)

    def postOrder(self, orders) -> None:

        def orderHandler(o:Order) -> None:
                logging.info(f'[TRADER] Posting {o.side} of {o.volume} @ {o.price}')
                response = requests.post(f'{self.url}/postOrder', json=o.model_dump())
                logging.info(f'[SERVER] Server replied: {response.json()}')
        
        if type(orders) == list:
            for o in orders:
                orderHandler(o)

        elif type(orders) == Order:
            orderHandler(orders)



    def getBookState(self,asDataframe:bool=False) -> dict[str,list[dict[str,int]]]:
        response = requests.get(f'{self.url}/orderBookState')
        logging.debug(f'[SERVER] Server replied: {response.json()}')
        
        json = response.json()

        cleanedAsks = self.cleanBookSide(json.get('asks',[]))
        cleanedBids = self.cleanBookSide(json.get('bids',[]))

        returnData = {'asks':cleanedAsks,'bids':cleanedBids,'fills':json.get('fills',[])}

        if not asDataframe:
            return returnData

        else:

            
            
            sellSide = returnData.get('asks',[])
            asksDf = pd.DataFrame(sellSide)
            
            buySide = returnData.get('bids',[])
            bidsDf = pd.DataFrame(buySide)

            fills = returnData.get('fills',[])
            fillsDf = pd.DataFrame(fills)

            return {'asks':asksDf,'bids':bidsDf,'fills':fillsDf}


    def cleanBookSide(self,book:list[list[dict]]) -> list[dict]:
        cleanedSide = []
        for priceLevel in book:
            orders = priceLevel[1]
            cleanedSide+=orders

        return cleanedSide