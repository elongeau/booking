FROM ubuntu:19.04 as app

RUN apt-get update 
RUN apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb
RUN dpkg -i /libgmp.deb && rm /libgmp.deb

RUN apt-get install -y postgresql
RUN apt-get install -y libc6

RUN mkdir /opt/app
WORKDIR /opt/app

COPY booking-reservation-exe /opt/app/booking-reservation-exe
EXPOSE 8080
CMD ["/opt/app/booking-reservation-exe", "8080"]
