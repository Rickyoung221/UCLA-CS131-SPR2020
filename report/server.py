import asyncio
import time
import sys
import aiohttp
import json

server_ports = {
    'Goloman': 12260,
    'Hands': 12261,
    'Holiday': 12262,
    'Welsh': 12263,
    'Wilkes': 12264
}

server_communication = {
	'Hill': ['Jaquez', 'Smith'],
	'Singleton': ['Smith', 'Campbell', 'Jaquez'],
	'Jaquez': ['Hill', 'Singelton'],
	'Smith': ['Hill', 'Singleton', 'Campbell'],
	'Campbell': ['Smith', 'Singleton']
}
client_locations = {}

def get_lat_lng(string):
    pair = string.replace('+', ' ').replace('-', ' -').strip().split(' ')
    return '%.7f' % float(pair[0]), '%.7f' % float(pair[1])

place_api = 'AIzaSyAe_tVvH0hkYN1O_CZg7H-ydyKc8Ktv-EU'
# implement a Server with asyncio ( from TA's slides DIS 1A)

class Server:
    async def main():
        server = await asyncio.start_server(handle_connection, host='127.0.0.1', port=12345)
        await server.serve_forever()

    async def handle_connection(reader, writer):
        data = await reader.readline()
        name = data.decode()
        greeting = "Hello, " + name
        writer.write(greeting.encode())
        await writer.drain()
        writer.close()

    if __name__ == '__main__':
        asyncio.run(main())



class Server:
    def __init__(self, name, ip='127.0.0.1', port=8888, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print("{} received {} from {}".format(self.name, message, addr))

        sendback_message = message

        print("{} send: {}".format(self.name, sendback_message))
        writer.write(sendback_message.encode())
        await writer.drain()

        print("close the client socket")
        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()


def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    print("Hello, welcome to server {}".format(args.server_name))

    server = Server(args.server_name)
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()