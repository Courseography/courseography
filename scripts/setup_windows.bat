:: This script must be run as an administrator.

:: First install Node.js and Yarn.
choco install nodejs.install -y
choco install yarn -y

:: Next, install JavaScript dependencies through Yarn.
yarn install
