import asyncio
import aiohttp
import time
import statistics

API_URL = "http://127.0.0.1:8000"

async def post_order(session, side, volume, price):
    start = time.perf_counter()
    async with session.post(f'{API_URL}/postOrder', json={
        'side': side,
        'volume': volume,
        'price': price
    }) as response:
        data = await response.json()
        elapsed = time.perf_counter() - start
        if 'error' in data:  # catch soft errors
            raise Exception(data['error'])
        return elapsed

async def stress_test(num_orders=1000, concurrency=100):
    latencies = []
    errors = 0

    connector = aiohttp.TCPConnector(limit=concurrency)
    async with aiohttp.ClientSession(connector=connector) as session:

        # warm up
        await post_order(session, 'Ask', 100, 5)

        start = time.perf_counter()

        # fire concurrency orders at a time until we reach num_orders
        for batch_start in range(0, num_orders, concurrency):
            batch = [
                post_order(session, 'Ask', 100, 5)
                for _ in range(min(concurrency, num_orders - batch_start))
            ]
            results = await asyncio.gather(*batch, return_exceptions=True)

            for r in results:
                if isinstance(r, Exception):
                    errors += 1
                else:
                    latencies.append(r)

        total_time = time.perf_counter() - start

    print(f"\n===== Stress Test Results =====")
    print(f"Total orders:    {num_orders}")
    print(f"Concurrency:     {concurrency}")
    print(f"Total time:      {total_time:.2f}s")
    print(f"Errors:          {errors}")
    print(f"Throughput:      {num_orders / total_time:.0f} orders/sec")
    print(f"Latency mean:    {statistics.mean(latencies)*1000:.2f}ms")
    print(f"Latency median:  {statistics.median(latencies)*1000:.2f}ms")
    print(f"Latency p99:     {sorted(latencies)[int(len(latencies)*0.99)]*1000:.2f}ms")
    print(f"Latency max:     {max(latencies)*1000:.2f}ms")

asyncio.run(stress_test(num_orders=100000, concurrency=100))