#!/usr/bin/env node

import * as fs   from 'node:fs'
import * as cp   from 'node:child_process'
import * as path from 'node:path'

const WATCH_DIRS    = ['./site', './src', './utils']
const BUILD_COMMAND = './cmd.sh'
const BUILD_ARGS    = ['build_wasm']

let is_building  = false
let build_queued = false

function run_build(): Promise<void> {
	return new Promise((resolve, reject) => {
		if (is_building) {
			build_queued = true
			resolve()
			return
		}

		is_building = true
		console.log('\n🔨 Building WASM...')
		
		let build_process = cp.spawn(BUILD_COMMAND, BUILD_ARGS, {
			stdio: 'inherit',
			shell: true
		})

		build_process.on('close', (code) => {
			is_building = false
			
			if (code === 0) {
				console.log('✅ Build completed successfully')
			} else {
				console.log(`❌ Build failed with exit code ${code}`)
			}

			// If a build was queued while this one was running, start it now
			if (build_queued) {
				build_queued = false
				run_build().then(resolve).catch(reject)
			} else {
				resolve()
			}
		})

		build_process.on('error', (error) => {
			is_building = false
			console.error('❌ Build error:', error)
			reject(error)
		})
	})
}

function debounce<T extends (...args: any[]) => any>(
	func: T,
	delay: number
): (...args: Parameters<T>) => void {
	let timeoutId: NodeJS.Timeout
	return (...args: Parameters<T>) => {
		clearTimeout(timeoutId)
		timeoutId = setTimeout(() => func(...args), delay)
	}
}

const debounced_build = debounce(() => {
	run_build().catch(console.error)
}, 100)

function start_watching() {
	console.log('👀 Watching for changes in:')
	
	for (let dir of WATCH_DIRS) {
		try {
			console.log(`\t- ${dir}`)
			
			let watcher = fs.watch(dir, {recursive: true}, (event_type, filename) => {
				if (filename != null && filename.endsWith('.odin')) {
					console.log(`📝 ${event_type}: ${path.join(dir, filename)}`)
					debounced_build()
				}
			})

			watcher.on('error', (error) => {
				console.error(`❌ Watcher error for ${dir}:`, error)
			})
			
		} catch (error) {
			console.error(`❌ Failed to watch ${dir}:`, error)
		}
	}
}

async function main() {
	console.log('🚀 Starting WASM build system...')
	
	// Initial build
	try {
		await run_build()
	} catch (error) {
		console.error('❌ Initial build failed:', error)
		process.exit(1)
	}
	
	// Start watching for changes
	start_watching()
	
	console.log('\n🎯 Build system is running. Press Ctrl+C to stop.')
}

// Handle graceful shutdown
process.on('SIGINT', () => {
	console.log('\n👋 Shutting down build system...')
	process.exit(0)
})

process.on('SIGTERM', () => {
	console.log('\n👋 Shutting down build system...')
	process.exit(0)
})

main().catch((error) => {
	console.error('❌ Build system error:', error)
	process.exit(1)
})
