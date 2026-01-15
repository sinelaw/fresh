// TypeScript highlighting test
interface User {
    name: string;
    age: number;
    readonly id: string;
}

type Status = "active" | "inactive";

class UserManager {
    private users: Map<string, User> = new Map();

    constructor(public readonly serviceName: string) {}

    async addUser(user: User): Promise<void> {
        this.users.set(user.id, user);
        console.log(`Added user: ${user.name}`);
    }

    getUser(id: string): User | undefined {
        return this.users.get(id);
    }
}

// Generic function
function identity<T>(arg: T): T {
    return arg;
}

// Arrow function with type annotation
const double = (x: number): number => x * 2;

// Constants and variables
const PI = 3.14159;
let counter = 0;
const isEnabled: boolean = true;
const items: string[] = ["one", "two", "three"];

// Async/await
async function fetchData(url: string): Promise<Response> {
    const response = await fetch(url);
    if (!response.ok) {
        throw new Error(`HTTP error: ${response.status}`);
    }
    return response;
}

// Export and import keywords
export { UserManager, User, Status };
export default UserManager;
