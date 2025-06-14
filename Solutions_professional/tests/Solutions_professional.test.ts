import { describe, test, expect, beforeEach } from 'vitest'

// Mock state
let performers: Record<string, any>
let performances: Record<number, any>
let performanceCounter: number

beforeEach(() => {
  performers = {}
  performances = {}
  performanceCounter = 0
})

// Simulate register-performer
function registerPerformer(sender: string, name: string, hourlyRate: number) {
  performers[sender] = {
    name,
    hourly_rate: hourlyRate,
    total_earned: 0,
    active: true
  }
  return true
}

// Simulate schedule-performance
function schedulePerformance(sender: string, venue: string, duration: number, date: number) {
  const performer = performers[sender]
  if (!performer || !performer.active) return 'ERR-PERFORMER-NOT-FOUND'
  if (duration <= 0) return 'ERR-INVALID-DURATION'

  const paymentAmount = duration * performer.hourly_rate
  performanceCounter += 1
  performances[performanceCounter] = {
    performer: sender,
    venue,
    duration,
    payment_amount: paymentAmount,
    payment_status: 'scheduled',
    date
  }

  return performanceCounter
}

// Simulate get-performer-details
function getPerformerDetails(address: string) {
  return performers[address] || {
    name: '',
    hourly_rate: 0,
    total_earned: 0,
    active: false
  }
}

describe('Performer Contract Logic (Mocked)', () => {
  test('should register a performer', () => {
    const result = registerPerformer('wallet_1', 'Alice', 100)
    expect(result).toBe(true)

    const performer = getPerformerDetails('wallet_1')
    expect(performer.name).toBe('Alice')
    expect(performer.hourly_rate).toBe(100)
    expect(performer.total_earned).toBe(0)
    expect(performer.active).toBe(true)
  })

  test('should schedule a performance with valid data', () => {
    registerPerformer('wallet_1', 'Alice', 100)
    const perfId = schedulePerformance('wallet_1', 'MainStage', 2, 20250516)

    expect(typeof perfId).toBe('number')
    const performance = performances[perfId]
    expect(performance.venue).toBe('MainStage')
    expect(performance.payment_amount).toBe(200)
    expect(performance.payment_status).toBe('scheduled')
  })

  test('should fail to schedule if duration is invalid', () => {
    registerPerformer('wallet_1', 'Alice', 100)
    const result = schedulePerformance('wallet_1', 'MainStage', 0, 20250516)
    expect(result).toBe('ERR-INVALID-DURATION')
  })

  test('should fail if performer not found', () => {
    const result = schedulePerformance('wallet_unknown', 'MainStage', 2, 20250516)
    expect(result).toBe('ERR-PERFORMER-NOT-FOUND')
  })

  test('should return default performer details if not found', () => {
    const result = getPerformerDetails('nonexistent')
    expect(result).toEqual({
      name: '',
      hourly_rate: 0,
      total_earned: 0,
      active: false
    })
  })
})
