import yfinance as yf
import os

if '__main__'== __name__:
    basket = [
        'NVDA', 'AAPL', 'MSFT', 'AMZN', 'GOOGL', 'GOOG', 'META', 'TSLA', 'BRK-B', 'WMT',
        'LLY', 'JPM', 'V', 'ORCL', 'XOM', 'JNJ', 'MA', 'NFLX', 'PLTR', 'ABBV',
        'COST', 'BAC', 'AMD', 'HD', 'PG', 'GE', 'CSCO', 'CVX', 'KO', 'UNH',
        'IBM', 'WFC', 'CAT', 'MS', 'AXP', 'MU', 'GS', 'MRK', 'CRM', 'TMUS',
        'PM', 'APP', 'RTX', 'MCD', 'ABT', 'TMO', 'AMAT', 'ISRG', 'PEP', 'LRCX'
    ]

    outputDir = "./data/cachedData"

    for filename in os.listdir(outputDir):
        os.remove(os.path.join(outputDir,filename))

    for ticker in basket:
        yf_ticker = yf.Ticker(ticker)
        data = yf_ticker.history(period='5y')
        data.to_csv(os.path.join(outputDir,f'{ticker}.csv'))