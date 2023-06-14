test('root level test', () => {
    expect(1).toBe(1);
})

it.only('root level with only', () => {
    expect(1).toBe(1);
})

it.each([1, 2, 3])('this is trickier (%#)', (n) => {
    expect(n).toBe(n);
})

describe('namespace', () => {
    test('root level test', () => {
        expect(1).toBe(1);
    })

    it.only('root level with only', () => {
        expect(1).toBe(1);
    })

    it.each([1, 2, 3])('this is trickier (%#)', (n) => {
        expect(n).toBe(n);
    })

    describe('nested namespace', () => {
        test('root level test', () => {
            expect(1).toBe(1);
        })

        it.only('root level with only', () => {
            expect(1).toBe(1);
        })

        it.each([1, 2, 3])('this is trickier (%#)', (n) => {
            expect(n).toBe(n);
        })
    })}
)
