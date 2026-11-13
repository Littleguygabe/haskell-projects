import asyncio
import aiohttp
import time
import statistics
import random

API_URL = "http://127.0.0.1:8000"
DURATION = 600  # 10 minutes in seconds
CONCURRENCY = 100

async def post_order(session, side, volume, price):
    start = time.perf_counter()
    async with session.post(f'{API_URL}/postOrder', json={
        'side': side,
        'volume': volume,
        'price': price
    }) as response:
        data = await response.json()
        elapsed = time.perf_counter() - start
        if 'error' in data:
            raise Exception(data['error'])
        return elapsed

def random_order():
    side = random.choice(['Bid', 'Ask'])
    price = random.randint(95, 105)  # tight spread around 100 to force lots of matching
    volume = random.randint(1, 200)
    return side, volume, price

async def worker(session, results, errors, stop_event):
    while not stop_event.is_set():
        side, volume, price = random_order()
        try:
            latency = await post_order(session, side, volume, price)
            results.append(latency)
        except Exception as e:
            errors.append(str(e))

async def print_stats(results, errors, stop_event, interval=30):
    start = time.perf_counter()
    while not stop_event.is_set():
        await asyncio.sleep(interval)
        elapsed = time.perf_counter() - start
        if not results:
            continue

        recent = results[-1000:]  # stats over last 1000 orders
        print(f"\n--- {elapsed:.0f}s elapsed ---")
        print(f"Total orders:   {len(results)}")
        print(f"Total errors:   {len(errors)}")
        print(f"Throughput:     {len(results) / elapsed:.0f} orders/sec")
        print(f"Latency mean:   {statistics.mean(recent)*1000:.2f}ms")
        print(f"Latency median: {statistics.median(recent)*1000:.2f}ms")
        print(f"Latency p99:    {sorted(recent)[int(len(recent)*0.99)]*1000:.2f}ms")
        print(f"Latency max:    {max(recent)*1000:.2f}ms")

async def run_simulation():
    results = []
    errors = []
    stop_event = asyncio.Event()

    connector = aiohttp.TCPConnector(limit=CONCURRENCY)
    async with aiohttp.ClientSession(connector=connector) as session:

        # warm up
        print("Warming up...")
        for _ in range(10):
            side, volume, price = random_order()
            await post_order(session, side, volume, price)

        print(f"Running for {DURATION // 60} minutes with {CONCURRENCY} concurrent workers...\n")

        # spawn workers and stats printer
        workers = [asyncio.create_task(worker(session, results, errors, stop_event))
                   for _ in range(CONCURRENCY)]
        stats_task = asyncio.create_task(print_stats(results, errors, stop_event))

        # run for the duration
        await asyncio.sleep(DURATION)
        stop_event.set()

        await asyncio.gather(*workers, return_exceptions=True)
        stats_task.cancel()

    # final report
    elapsed = DURATION
    print(f"\n===== Final Results ({DURATION // 60} min simulation) =====")
    print(f"Total orders:   {len(results)}")
    print(f"Total errors:   {len(errors)}")
    print(f"Throughput:     {len(results) / elapsed:.0f} orders/sec")
    print(f"Latency mean:   {statistics.mean(results)*1000:.2f}ms")
    print(f"Latency median: {statistics.median(results)*1000:.2f}ms")
    print(f"Latency p99:    {sorted(results)[int(len(results)*0.99)]*1000:.2f}ms")
    print(f"Latency max:    {max(results)*1000:.2f}ms")

    if errors:
        print(f"\nError breakdown:")
        error_counts = {}
        for e in errors:
            error_counts[e] = error_counts.get(e, 0) + 1
        for msg, count in error_counts.items():
            print(f"  {count}x {msg}")

asyncio.run(run_simulation())