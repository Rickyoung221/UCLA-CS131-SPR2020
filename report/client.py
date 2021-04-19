import asyncio
import time
import sys

# implement a client with asyncio ( from TA's slides DIS 1A)
class Client:
    def __init__(self, ip='127.0.0.1', port=8888, name='client', message_max_length=1e6):
        """
        127.0.0.1 is the localhost
        port could be any port
        """
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        """
        on client side send the message for echo
        """
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print(f'{self.name} send: {message!r}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} received: {data.decode()!r}')

        print('close the socket')
        # The following lines closes the stream properly
        # If there is any warning, it's due to a bug o Python 3.8: https://bugs.python.org/issue38529
        # Please ignore it
        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                asyncio.run(self.tcp_echo_client(message))


if __name__ == '__main__':
    client = Client()  # using the default settings
    client.run_until_quit()








async def setup_tcp_client(message, loop):
    reader, writer = await asyncio.open_connection('127.0.0.1', 12260, loop=loop)
    print("Sending:", message, end="")
    writer.write(message.encode())
    data = await reader.read(10000)
    print("Received:", data.decode(), end="")
    writer.close()

def main():
    message = "WHATSAT kiwi.cs.ucla.edu 10 5\n"
    loop = asyncio.get_event_loop()
    loop.run_until_complete(setup_tcp_client(message, loop))
    loop.close()

if __name__ == '__main__':
    asyncio.run(main())
