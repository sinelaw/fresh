// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract HelloWorld {
    string public greeting;
    address public owner;
    uint256 public count;

    event GreetingChanged(string newGreeting, address changer);

    modifier onlyOwner() {
        require(msg.sender == owner, "Not owner");
        _;
    }

    constructor(string memory _greeting) {
        greeting = _greeting;
        owner = msg.sender;
        count = 0;
    }

    function setGreeting(string memory _greeting) public onlyOwner {
        greeting = _greeting;
        count += 1;
        emit GreetingChanged(_greeting, msg.sender);
    }
}
