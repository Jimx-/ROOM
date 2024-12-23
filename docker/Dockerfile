FROM debian:bookworm

USER root

ENV PYTHON_VERSION 3.11.9
ENV YOSYS_VERSION  0.43
ENV AMARANTH_REV   d218273   

RUN apt-get update && \
    apt-get install -y build-essential gdb git lcov libbz2-dev libffi-dev \
    libgdbm-dev liblzma-dev libncurses5-dev libreadline6-dev libsqlite3-dev \
    libssl-dev lzma lzma-dev tk-dev uuid-dev wget xvfb zlib1g-dev \
    clang lld bison flex gawk tcl-dev graphviz xdot pkg-config \
    libboost-system-dev libboost-python-dev libboost-filesystem-dev

RUN wget https://www.python.org/ftp/python/$PYTHON_VERSION/Python-$PYTHON_VERSION.tgz && \
    tar xvf Python-$PYTHON_VERSION.tgz && \
    cd Python-$PYTHON_VERSION && \
    ./configure && \
    make -j && \
    make install && \
    ln -s /usr/local/bin/python3 /usr/bin/python && \
    cd .. && \
    rm -rf Python-$PYTHON_VERSION*

COPY amaranth.patch .

RUN git clone https://github.com/amaranth-lang/amaranth.git && \
    cd amaranth && \
    git checkout -q $AMARANTH_REV && \
    patch -p1 < ../amaranth.patch && \
    pip3 install . && \
    cd .. && \
    rm -rf amaranth amaranth.patch

COPY requirements.txt .

RUN pip3 install -r requirements.txt

RUN git clone https://github.com/YosysHQ/yosys.git && \
    cd yosys && \
    git checkout yosys-$YOSYS_VERSION && \
    git submodule update --init --recursive && \
    make -j8 && \
    make install && \
    cd .. && \
    rm -rf yosys
    
