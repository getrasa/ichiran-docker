import sys
def handler(event, context):
    print("runs", event, context)
    return 'Hello from AWS Lambda using Python' + sys.version + '!'   