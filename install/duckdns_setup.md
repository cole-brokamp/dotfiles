## Duck DNS

- Duck DNS is a free alternative to dynaDNS used to forward a URL to your dynamic external ip address
- The installed script sends the external ip address to duck DNS
- Setup a crontab job to do this every 5 minutes
- Register an account and domain at www.duckdns.org

### Installing

#### Requirements

- make sure a crontab is running
    `ps -ef | grep cr[o]n`
- make sure curl is installed
    `curl`

#### Installation

- `mkdir .duckdns`
- `cd .duckdns`
- `nano duck.sh`
- paste `echo url="https://www.duckdns.org/update?domains=<DOMAIN>&token=<TOKEN>&ip=" | curl -k -o ~/.duckdns/duck.log -K -`
into the file, replacing `<TOKEN>` with your token and `<DOMAIN>` with your domain from duckdns.org
- `chmod 700 duck.sh`
- edit crontab (`crontab -e`) and paste `*/5 * * * * ~/.duckdns/duck.sh >/dev/null 2>&1` into it

#### Testing

- `./duck.sh`
- `cat duck.log` (should see `OK`)
