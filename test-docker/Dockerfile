FROM ubuntu
LABEL Damian <getrasa4@gmail.com>

ARG DEBIAN_FRONTEND=noninteractive


# Update packages
RUN apt update; apt dist-upgrade -y

# Install packages
RUN apt install -y python3 python3-pip 
RUN pip3 install awslambdaric --target ./

COPY app.py ./
# Install python packages
# RUN pip3 install --upgrade pip
# COPY requirements.txt .
# RUN pip3 install -r requirements.txt --target .
# RUN pip3 install -r requirements.txt --target /usr/local/lib/python3.8/dist-packages/

# ENTRYPOINT ["app.handler"]
ENTRYPOINT [ "/usr/bin/python3", "-m", "awslambdaric" ]
CMD [ "app.handler" ]