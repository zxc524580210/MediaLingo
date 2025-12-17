//
//  ViewController.swift
//  MediaLingo
//
//  Created by zxc on 2025/12/17.
//

import Cocoa
import CoreAudio
import AudioToolbox
import Accelerate
import ScreenCaptureKit

// MARK: - Audio Capture Protocol

protocol AudioCaptureProvider {
    var onAudioData: ((_ buffer: UnsafePointer<Float>, _ frameCount: UInt32) -> Void)? { get set }
    var sampleRate: Double { get }
    var channelCount: UInt32 { get }
    func start(completion: @escaping (Error?) -> Void)
    func stop()
}

// MARK: - Audio Capture Method

enum AudioCaptureMethod: Int, CaseIterable {
    case screenCaptureKit = 0
    case blackHoleVirtualDevice = 1
    
    var displayName: String {
        switch self {
        case .screenCaptureKit:
            return "ScreenCaptureKit (Recommended)"
        case .blackHoleVirtualDevice:
            return "BlackHole Virtual Device"
        }
    }
    
    var description: String {
        switch self {
        case .screenCaptureKit:
            return "Uses macOS built-in system audio capture (macOS 13+)"
        case .blackHoleVirtualDevice:
            return "Uses BlackHole virtual audio device"
        }
    }
}

class ViewController: NSViewController {
    
    private var audioCapture: AudioCaptureProvider?
    private var currentMethod: AudioCaptureMethod = .screenCaptureKit
    
    private var startButton: NSButton!
    private var stopButton: NSButton!
    private var spectrumView: SpectrumView!
    private var methodPopup: NSPopUpButton!
    private var methodDescriptionLabel: NSTextField!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        // 设置界面
        setupUI()
        
        // 初始化音频捕获（默认使用 ScreenCaptureKit）
        setupAudioCapture(method: currentMethod)
    }
    
    private func setupAudioCapture(method: AudioCaptureMethod) {
        // 停止现有捕获
        audioCapture?.stop()
        
        // 根据选择的方法创建捕获器
        switch method {
        case .screenCaptureKit:
            if #available(macOS 13.0, *) {
                audioCapture = ScreenCaptureKitAudioCapture()
            } else {
                // 如果系统版本不支持，回退到 BlackHole
                showAlert(title: "ScreenCaptureKit Not Available",
                         message: "ScreenCaptureKit requires macOS 13.0 or later. Falling back to BlackHole.")
                audioCapture = BlackHoleAudioCapture()
                currentMethod = .blackHoleVirtualDevice
                methodPopup.selectItem(at: currentMethod.rawValue)
            }
        case .blackHoleVirtualDevice:
            audioCapture = BlackHoleAudioCapture()
        }
        
        // 设置音频数据回调
        audioCapture?.onAudioData = { [weak self] buffer, frameCount in
            guard let self = self else { return }
            
            // 获取通道数
            let channelCount = Int(self.audioCapture?.channelCount ?? 2)
            
            // 打印原始信号值（前几个采样点）
            let samplesToShow = min(Int(frameCount) * channelCount, 10)
            var rawValues: [Float] = []
            for i in 0..<samplesToShow {
                rawValues.append(buffer[i])
            }
            
            // 计算信号统计信息
            var minVal: Float = 0
            var maxVal: Float = 0
            var avgVal: Float = 0
            let totalSamples = Int(frameCount) * channelCount
            
            vDSP_minv(buffer, 1, &minVal, vDSP_Length(totalSamples))
            vDSP_maxv(buffer, 1, &maxVal, vDSP_Length(totalSamples))
            vDSP_meanv(buffer, 1, &avgVal, vDSP_Length(totalSamples))
            
            print("Audio Buffer - Frames: \(frameCount), Channels: \(channelCount)")
            print("  Raw samples (first \(samplesToShow)): \(rawValues)")
            print("  Min: \(minVal), Max: \(maxVal), Avg: \(avgVal)")
            
            // 将 interleaved 数据转换为单声道（取左声道或混合）
            var monoData = [Float](repeating: 0, count: Int(frameCount))
            for i in 0..<Int(frameCount) {
                // 混合所有通道
                var sum: Float = 0
                for ch in 0..<channelCount {
                    sum += buffer[i * channelCount + ch]
                }
                monoData[i] = sum / Float(channelCount)
            }
            
            // 在主线程更新频谱显示
            DispatchQueue.main.async {
                self.spectrumView.updateSpectrum(with: monoData)
            }
        }
        
        // 更新描述标签
        methodDescriptionLabel?.stringValue = method.description
    }
    
    private func setupUI() {
        // 设置视图的最小尺寸
        view.setContentHuggingPriority(.defaultLow, for: .horizontal)
        view.setContentHuggingPriority(.defaultLow, for: .vertical)
        
        // 创建频谱视图
        spectrumView = SpectrumView()
        spectrumView.translatesAutoresizingMaskIntoConstraints = false
        spectrumView.wantsLayer = true
        spectrumView.layer?.backgroundColor = NSColor.black.cgColor
        spectrumView.layer?.cornerRadius = 8
        // 设置压缩阻力，防止被过度压缩
        spectrumView.setContentCompressionResistancePriority(.defaultLow, for: .vertical)
        view.addSubview(spectrumView)
        
        // 创建控制容器视图
        let controlContainer = NSView()
        controlContainer.translatesAutoresizingMaskIntoConstraints = false
        controlContainer.setContentCompressionResistancePriority(.required, for: .vertical)
        view.addSubview(controlContainer)
        
        // 创建方法选择标签
        let methodLabel = NSTextField(labelWithString: "Capture Method:")
        methodLabel.translatesAutoresizingMaskIntoConstraints = false
        methodLabel.font = NSFont.systemFont(ofSize: 13, weight: .medium)
        controlContainer.addSubview(methodLabel)
        
        // 创建方法选择下拉菜单
        methodPopup = NSPopUpButton()
        methodPopup.translatesAutoresizingMaskIntoConstraints = false
        for method in AudioCaptureMethod.allCases {
            methodPopup.addItem(withTitle: method.displayName)
        }
        methodPopup.selectItem(at: currentMethod.rawValue)
        methodPopup.target = self
        methodPopup.action = #selector(captureMethodChanged(_:))
        controlContainer.addSubview(methodPopup)
        
        // 创建方法描述标签
        methodDescriptionLabel = NSTextField(labelWithString: currentMethod.description)
        methodDescriptionLabel.translatesAutoresizingMaskIntoConstraints = false
        methodDescriptionLabel.font = NSFont.systemFont(ofSize: 11)
        methodDescriptionLabel.textColor = .secondaryLabelColor
        controlContainer.addSubview(methodDescriptionLabel)
        
        // 创建分隔线
        let separator = NSBox()
        separator.boxType = .separator
        separator.translatesAutoresizingMaskIntoConstraints = false
        controlContainer.addSubview(separator)
        
        // 创建按钮容器
        let buttonStack = NSStackView()
        buttonStack.translatesAutoresizingMaskIntoConstraints = false
        buttonStack.orientation = .horizontal
        buttonStack.spacing = 12
        controlContainer.addSubview(buttonStack)
        
        // 创建 Start 按钮
        startButton = NSButton(title: "Start Capture", target: self, action: #selector(startCapture(_:)))
        startButton.bezelStyle = .rounded
        startButton.setContentHuggingPriority(.required, for: .vertical)
        buttonStack.addArrangedSubview(startButton)
        
        // 创建 Stop 按钮
        stopButton = NSButton(title: "Stop Capture", target: self, action: #selector(stopCapture(_:)))
        stopButton.bezelStyle = .rounded
        stopButton.isEnabled = false // 初始禁用
        stopButton.setContentHuggingPriority(.required, for: .vertical)
        buttonStack.addArrangedSubview(stopButton)
        
        // 设置约束
        NSLayoutConstraint.activate([
            // 频谱视图在顶部，动态高度
            spectrumView.topAnchor.constraint(equalTo: view.topAnchor, constant: 20),
            spectrumView.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: 20),
            spectrumView.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -20),
            // 最小高度约束
            spectrumView.heightAnchor.constraint(greaterThanOrEqualToConstant: 100),
            
            // 控制容器在底部
            controlContainer.topAnchor.constraint(equalTo: spectrumView.bottomAnchor, constant: 20),
            controlContainer.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: 20),
            controlContainer.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -20),
            controlContainer.bottomAnchor.constraint(equalTo: view.bottomAnchor, constant: -20),
            
            // 方法选择标签
            methodLabel.topAnchor.constraint(equalTo: controlContainer.topAnchor),
            methodLabel.leadingAnchor.constraint(equalTo: controlContainer.leadingAnchor),
            
            // 方法选择下拉菜单
            methodPopup.centerYAnchor.constraint(equalTo: methodLabel.centerYAnchor),
            methodPopup.leadingAnchor.constraint(equalTo: methodLabel.trailingAnchor, constant: 8),
            methodPopup.widthAnchor.constraint(greaterThanOrEqualToConstant: 220),
            
            // 方法描述标签
            methodDescriptionLabel.topAnchor.constraint(equalTo: methodLabel.bottomAnchor, constant: 4),
            methodDescriptionLabel.leadingAnchor.constraint(equalTo: controlContainer.leadingAnchor),
            methodDescriptionLabel.trailingAnchor.constraint(equalTo: controlContainer.trailingAnchor),
            
            // 分隔线
            separator.topAnchor.constraint(equalTo: methodDescriptionLabel.bottomAnchor, constant: 12),
            separator.leadingAnchor.constraint(equalTo: controlContainer.leadingAnchor),
            separator.trailingAnchor.constraint(equalTo: controlContainer.trailingAnchor),
            
            // 按钮容器
            buttonStack.topAnchor.constraint(equalTo: separator.bottomAnchor, constant: 12),
            buttonStack.centerXAnchor.constraint(equalTo: controlContainer.centerXAnchor),
            buttonStack.bottomAnchor.constraint(equalTo: controlContainer.bottomAnchor)
        ])
    }
    
    override func viewDidLayout() {
        super.viewDidLayout()
        // 确保频谱视图在布局变化时重新绘制
        spectrumView.needsDisplay = true
    }
    
    @objc private func captureMethodChanged(_ sender: NSPopUpButton) {
        guard let newMethod = AudioCaptureMethod(rawValue: sender.indexOfSelectedItem) else { return }
        
        // 如果正在捕获，先停止
        let wasCapturing = !startButton.isEnabled
        if wasCapturing {
            stopCapture(sender)
        }
        
        currentMethod = newMethod
        setupAudioCapture(method: newMethod)
        
        // 更新描述
        methodDescriptionLabel.stringValue = newMethod.description
    }
    
    @IBAction func startCapture(_ sender: Any) {
        // 禁用按钮，防止重复点击
        startButton.isEnabled = false
        methodPopup.isEnabled = false
        
        // 使用异步方式启动捕获
        audioCapture?.start { [weak self] error in
            DispatchQueue.main.async {
                guard let self = self else { return }
                
                if let error = error {
                    print("Failed to start capture: \(error)")
                    
                    // 恢复按钮状态
                    self.startButton.isEnabled = true
                    self.methodPopup.isEnabled = true
                    
                    // 显示错误提示
                    self.showAlert(title: "Failed to Start Capture", message: error.localizedDescription)
                } else {
                    print("Audio capture started using \(self.currentMethod.displayName)")
                    
                    // 更新按钮状态
                    self.stopButton.isEnabled = true
                }
            }
        }
    }
    
    @IBAction func stopCapture(_ sender: Any) {
        audioCapture?.stop()
        print("Audio capture stopped")
        
        // 更新按钮状态
        startButton.isEnabled = true
        stopButton.isEnabled = false
        methodPopup.isEnabled = true // 停止后允许切换方法
        
        // 清除频谱显示
        spectrumView.clearSpectrum()
    }
    
    private func showAlert(title: String, message: String) {
        let alert = NSAlert()
        alert.messageText = title
        alert.informativeText = message
        alert.alertStyle = .warning
        alert.addButton(withTitle: "OK")
        alert.runModal()
    }

    override var representedObject: Any? {
        didSet {
        // Update the view, if already loaded.
        }
    }
}

// MARK: - Spectrum View

class SpectrumView: NSView {
    
    // FFT 设置
    private let fftSize = 2048
    private var fftSetup: vDSP_DFT_Setup?
    
    // 频谱数据
    private var spectrumData: [Float] = []
    private let barCount = 64 // 显示的频谱条数
    
    // 平滑处理
    private var smoothedSpectrum: [Float] = []
    private let smoothingFactor: Float = 0.3
    
    // 颜色渐变
    private let barColors: [NSColor] = {
        var colors: [NSColor] = []
        for i in 0..<64 {
            let hue = CGFloat(i) / 64.0 * 0.3 // 从红色到绿色
            colors.append(NSColor(hue: hue, saturation: 0.8, brightness: 0.9, alpha: 1.0))
        }
        return colors
    }()
    
    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setupFFT()
        smoothedSpectrum = [Float](repeating: 0, count: barCount)
    }
    
    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setupFFT()
        smoothedSpectrum = [Float](repeating: 0, count: barCount)
    }
    
    deinit {
        if let setup = fftSetup {
            vDSP_DFT_DestroySetup(setup)
        }
    }
    
    private func setupFFT() {
        fftSetup = vDSP_DFT_zop_CreateSetup(
            nil,
            vDSP_Length(fftSize),
            .FORWARD
        )
    }
    
    func updateSpectrum(with audioData: [Float]) {
        // 确保有足够的数据
        guard audioData.count >= fftSize else {
            // 如果数据不足，用零填充
            var paddedData = audioData
            paddedData.append(contentsOf: [Float](repeating: 0, count: fftSize - audioData.count))
            performFFT(on: paddedData)
            return
        }
        
        // 使用最新的数据
        let startIndex = audioData.count - fftSize
        let fftInput = Array(audioData[startIndex..<startIndex + fftSize])
        performFFT(on: fftInput)
    }
    
    private func performFFT(on input: [Float]) {
        guard let fftSetup = fftSetup else { return }
        
        // 应用汉宁窗
        var windowedInput = [Float](repeating: 0, count: fftSize)
        var window = [Float](repeating: 0, count: fftSize)
        vDSP_hann_window(&window, vDSP_Length(fftSize), Int32(vDSP_HANN_NORM))
        vDSP_vmul(input, 1, window, 1, &windowedInput, 1, vDSP_Length(fftSize))
        
        // 准备复数数据
        var realInput = windowedInput
        var imagInput = [Float](repeating: 0, count: fftSize)
        var realOutput = [Float](repeating: 0, count: fftSize)
        var imagOutput = [Float](repeating: 0, count: fftSize)
        
        // 执行 DFT
        vDSP_DFT_Execute(fftSetup, &realInput, &imagInput, &realOutput, &imagOutput)
        
        // 计算幅度
        var magnitudes = [Float](repeating: 0, count: fftSize / 2)
        realOutput.withUnsafeBufferPointer { realPtr in
            imagOutput.withUnsafeBufferPointer { imagPtr in
                var complex = DSPSplitComplex(
                    realp: UnsafeMutablePointer(mutating: realPtr.baseAddress!),
                    imagp: UnsafeMutablePointer(mutating: imagPtr.baseAddress!)
                )
                vDSP_zvabs(&complex, 1, &magnitudes, 1, vDSP_Length(fftSize / 2))
            }
        }
        
        // 将频谱数据分配到显示条
        var newSpectrum = [Float](repeating: 0, count: barCount)
        let binPerBar = (fftSize / 2) / barCount
        
        for i in 0..<barCount {
            let startBin = i * binPerBar
            let endBin = min(startBin + binPerBar, fftSize / 2)
            
            // 计算这个范围内的平均值
            var sum: Float = 0
            for j in startBin..<endBin {
                sum += magnitudes[j]
            }
            newSpectrum[i] = sum / Float(endBin - startBin)
        }
        
        // 归一化和对数缩放
        var maxVal: Float = 0
        vDSP_maxv(newSpectrum, 1, &maxVal, vDSP_Length(barCount))
        
        if maxVal > 0 {
            for i in 0..<barCount {
                // 对数缩放使小信号更明显
                let normalized = newSpectrum[i] / maxVal
                newSpectrum[i] = log10(1 + normalized * 9) // 对数缩放 0-1
            }
        }
        
        // 平滑处理
        for i in 0..<barCount {
            smoothedSpectrum[i] = smoothedSpectrum[i] * (1 - smoothingFactor) + newSpectrum[i] * smoothingFactor
            // 衰减效果
            if smoothedSpectrum[i] > newSpectrum[i] {
                smoothedSpectrum[i] = max(newSpectrum[i], smoothedSpectrum[i] - 0.05)
            }
        }
        
        spectrumData = smoothedSpectrum
        needsDisplay = true
    }
    
    func clearSpectrum() {
        spectrumData = [Float](repeating: 0, count: barCount)
        smoothedSpectrum = [Float](repeating: 0, count: barCount)
        needsDisplay = true
    }
    
    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        
        // 绘制背景
        context.setFillColor(NSColor.black.cgColor)
        context.fill(bounds)
        
        // 绘制频谱条
        let barWidth = bounds.width / CGFloat(barCount)
        let barSpacing: CGFloat = 2
        let effectiveBarWidth = barWidth - barSpacing
        let maxHeight = bounds.height - 20
        
        for i in 0..<min(spectrumData.count, barCount) {
            let barHeight = CGFloat(spectrumData[i]) * maxHeight
            let x = CGFloat(i) * barWidth + barSpacing / 2
            let y: CGFloat = 10
            
            // 绘制渐变色条
            let rect = CGRect(x: x, y: y, width: effectiveBarWidth, height: max(barHeight, 2))
            
            // 根据高度设置颜色
            let colorIndex = min(i, barColors.count - 1)
            let color = barColors[colorIndex]
            
            // 创建渐变
            let startColor = color.withAlphaComponent(0.8)
            let endColor = color.withAlphaComponent(1.0)
            
            context.saveGState()
            context.clip(to: rect)
            
            let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [startColor.cgColor, endColor.cgColor] as CFArray,
                locations: [0, 1]
            )!
            
            context.drawLinearGradient(
                gradient,
                start: CGPoint(x: rect.midX, y: rect.minY),
                end: CGPoint(x: rect.midX, y: rect.maxY),
                options: []
            )
            
            context.restoreGState()
            
            // 绘制顶部高亮
            if barHeight > 5 {
                context.setFillColor(NSColor.white.withAlphaComponent(0.8).cgColor)
                let topRect = CGRect(x: x, y: y + barHeight - 3, width: effectiveBarWidth, height: 3)
                context.fill(topRect)
            }
        }
        
        // 如果没有数据，显示提示
        if spectrumData.isEmpty || spectrumData.allSatisfy({ $0 == 0 }) {
            let text = "Click 'Start Capture' to view spectrum"
            let attributes: [NSAttributedString.Key: Any] = [
                .foregroundColor: NSColor.gray,
                .font: NSFont.systemFont(ofSize: 14)
            ]
            let size = text.size(withAttributes: attributes)
            let point = CGPoint(
                x: (bounds.width - size.width) / 2,
                y: (bounds.height - size.height) / 2
            )
            text.draw(at: point, withAttributes: attributes)
        }
    }
}

// MARK: - ScreenCaptureKit Audio Capture

@available(macOS 13.0, *)
class ScreenCaptureKitAudioCapture: NSObject, AudioCaptureProvider, SCStreamDelegate, SCStreamOutput {
    
    private var stream: SCStream?
    private var streamConfiguration: SCStreamConfiguration?
    
    /// 音频数据回调
    var onAudioData: ((_ buffer: UnsafePointer<Float>, _ frameCount: UInt32) -> Void)?
    
    /// 音频格式信息
    private(set) var sampleRate: Double = 48000.0
    private(set) var channelCount: UInt32 = 2
    
    override init() {
        super.init()
    }
    
    deinit {
        stop()
    }
    
    // MARK: - AudioCaptureProvider
    
    func start(completion: @escaping (Error?) -> Void) {
        Task {
            do {
                try await startAsync()
                completion(nil)
            } catch {
                completion(error)
            }
        }
    }
    
    private func startAsync() async throws {
        // 获取可共享内容
        let content = try await SCShareableContent.excludingDesktopWindows(false, onScreenWindowsOnly: false)
        
        // 获取主显示器（我们只需要音频，但需要指定一个显示器）
        guard let display = content.displays.first else {
            throw ScreenCaptureError.noDisplayFound
        }
        
        // 创建内容过滤器 - 捕获整个显示器（包括系统音频）
        let filter = SCContentFilter(display: display, excludingWindows: [])
        
        // 配置流
        let configuration = SCStreamConfiguration()
        
        // 禁用视频捕获（我们只需要音频）
        configuration.width = 2
        configuration.height = 2
        configuration.minimumFrameInterval = CMTime(value: 1, timescale: 1) // 最低帧率
        
        // 启用音频捕获
        configuration.capturesAudio = true
        configuration.excludesCurrentProcessAudio = true // 排除当前应用的音频，避免反馈
        configuration.sampleRate = Int(sampleRate)
        configuration.channelCount = Int(channelCount)
        
        self.streamConfiguration = configuration
        
        // 创建流
        let stream = SCStream(filter: filter, configuration: configuration, delegate: self)
        self.stream = stream
        
        // 添加音频输出
        try stream.addStreamOutput(self, type: .audio, sampleHandlerQueue: DispatchQueue(label: "com.medialingo.screencapture.audio"))
        
        // 开始捕获
        try await stream.startCapture()
        
        print("ScreenCaptureKit audio capture started")
    }
    
    func stop() {
        guard let stream = stream else { return }
        
        Task {
            do {
                try await stream.stopCapture()
                print("ScreenCaptureKit audio capture stopped")
            } catch {
                print("Error stopping capture: \(error)")
            }
        }
        
        self.stream = nil
    }
    
    // MARK: - SCStreamOutput
    
    func stream(_ stream: SCStream, didOutputSampleBuffer sampleBuffer: CMSampleBuffer, of type: SCStreamOutputType) {
        guard type == .audio else { return }
        
        // 获取音频格式
        guard let formatDescription = sampleBuffer.formatDescription else { return }
        let audioFormat = CMAudioFormatDescriptionGetStreamBasicDescription(formatDescription)?.pointee
        
        if let format = audioFormat {
            // 更新采样率和通道数
            if format.mSampleRate != sampleRate {
                sampleRate = format.mSampleRate
            }
            if format.mChannelsPerFrame != channelCount {
                channelCount = format.mChannelsPerFrame
            }
        }
        
        // 获取音频数据
        guard let blockBuffer = sampleBuffer.dataBuffer else { return }
        
        var length = 0
        var dataPointer: UnsafeMutablePointer<Int8>?
        
        let status = CMBlockBufferGetDataPointer(blockBuffer, atOffset: 0, lengthAtOffsetOut: nil, totalLengthOut: &length, dataPointerOut: &dataPointer)
        
        guard status == kCMBlockBufferNoErr, let data = dataPointer else { return }
        
        // 转换为 Float 数据
        let frameCount = length / (Int(channelCount) * MemoryLayout<Float>.size)
        
        data.withMemoryRebound(to: Float.self, capacity: frameCount * Int(channelCount)) { floatPointer in
            onAudioData?(floatPointer, UInt32(frameCount))
        }
    }
    
    // MARK: - SCStreamDelegate
    
    func stream(_ stream: SCStream, didStopWithError error: Error) {
        print("ScreenCaptureKit stream stopped with error: \(error)")
    }
}

// MARK: - ScreenCaptureKit Errors

enum ScreenCaptureError: Error, LocalizedError {
    case noDisplayFound
    case permissionDenied
    case captureStartFailed
    
    var errorDescription: String? {
        switch self {
        case .noDisplayFound:
            return "No display found for screen capture"
        case .permissionDenied:
            return "Screen recording permission denied. Please enable it in System Settings > Privacy & Security > Screen Recording"
        case .captureStartFailed:
            return "Failed to start screen capture"
        }
    }
}

// MARK: - BlackHole Audio Capture API

class BlackHoleAudioCapture: AudioCaptureProvider {
    
    fileprivate var audioUnit: AudioComponentInstance?
    private var blackHoleDeviceID: AudioDeviceID = kAudioObjectUnknown
    
    /// 音频数据回调
    var onAudioData: ((_ buffer: UnsafePointer<Float>, _ frameCount: UInt32) -> Void)?
    
    /// 音频格式信息
    private(set) var sampleRate: Double = 48000.0
    private(set) var channelCount: UInt32 = 2
    
    init() {
        findBlackHoleDevice()
        
        // 如果找到设备，读取设备的实际采样率
        if blackHoleDeviceID != kAudioObjectUnknown {
            updateDeviceSampleRate()
        }
    }
    
    deinit {
        stop()
    }
    
    // MARK: - Public Methods
    
    /// 开始捕获音频
    func start(completion: @escaping (Error?) -> Void) {
        do {
            guard blackHoleDeviceID != kAudioObjectUnknown else {
                throw AudioCaptureError.deviceNotFound
            }
            
            try setupAudioUnit()
            
            let status = AudioOutputUnitStart(audioUnit!)
            guard status == noErr else {
                throw AudioCaptureError.startFailed(status)
            }
            
            completion(nil)
        } catch {
            completion(error)
        }
    }
    
    /// 停止捕获音频
    func stop() {
        if let unit = audioUnit {
            AudioOutputUnitStop(unit)
            AudioComponentInstanceDispose(unit)
            audioUnit = nil
        }
    }
    
    /// 获取所有可用的音频输入设备
    static func listInputDevices() -> [(id: AudioDeviceID, name: String)] {
        var propertyAddress = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        
        var dataSize: UInt32 = 0
        var status = AudioObjectGetPropertyDataSize(
            AudioObjectID(kAudioObjectSystemObject),
            &propertyAddress,
            0,
            nil,
            &dataSize
        )
        
        guard status == noErr else { return [] }
        
        let deviceCount = Int(dataSize) / MemoryLayout<AudioDeviceID>.size
        var deviceIDs = [AudioDeviceID](repeating: 0, count: deviceCount)
        
        status = AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject),
            &propertyAddress,
            0,
            nil,
            &dataSize,
            &deviceIDs
        )
        
        guard status == noErr else { return [] }
        
        var devices: [(id: AudioDeviceID, name: String)] = []
        
        for deviceID in deviceIDs {
            // 检查是否有输入通道
            var inputPropertyAddress = AudioObjectPropertyAddress(
                mSelector: kAudioDevicePropertyStreamConfiguration,
                mScope: kAudioDevicePropertyScopeInput,
                mElement: kAudioObjectPropertyElementMain
            )
            
            var inputSize: UInt32 = 0
            status = AudioObjectGetPropertyDataSize(deviceID, &inputPropertyAddress, 0, nil, &inputSize)
            
            if status == noErr && inputSize > 0 {
                let bufferListPointer = UnsafeMutablePointer<AudioBufferList>.allocate(capacity: 1)
                defer { bufferListPointer.deallocate() }
                
                status = AudioObjectGetPropertyData(deviceID, &inputPropertyAddress, 0, nil, &inputSize, bufferListPointer)
                
                if status == noErr {
                    let bufferList = bufferListPointer.pointee
                    if bufferList.mNumberBuffers > 0 {
                        if let name = getDeviceName(deviceID: deviceID) {
                            devices.append((id: deviceID, name: name))
                        }
                    }
                }
            }
        }
        
        return devices
    }
    
    // MARK: - Private Methods
    
    private func findBlackHoleDevice() {
        let devices = BlackHoleAudioCapture.listInputDevices()
        
        // 查找 BlackHole 2ch 设备
        for device in devices {
            if device.name.contains("BlackHole 2ch") {
                blackHoleDeviceID = device.id
                print("Found BlackHole 2ch device: \(device.name) (ID: \(device.id))")
                return
            }
        }
        
        print("BlackHole 2ch device not found. Available devices:")
        for device in devices {
            print("  - \(device.name) (ID: \(device.id))")
        }
    }
    
    /// 从设备读取实际采样率
    private func updateDeviceSampleRate() {
        var propertyAddress = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyNominalSampleRate,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        
        var sampleRate: Float64 = 0
        var dataSize = UInt32(MemoryLayout<Float64>.size)
        
        let status = AudioObjectGetPropertyData(
            blackHoleDeviceID,
            &propertyAddress,
            0,
            nil,
            &dataSize,
            &sampleRate
        )
        
        if status == noErr && sampleRate > 0 {
            self.sampleRate = sampleRate
            print("Device sample rate: \(sampleRate) Hz")
        }
    }
    
    private static func getDeviceName(deviceID: AudioDeviceID) -> String? {
        var propertyAddress = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyDeviceNameCFString,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        
        var name: CFString?
        var dataSize = UInt32(MemoryLayout<CFString?>.size)
        
        let status = AudioObjectGetPropertyData(
            deviceID,
            &propertyAddress,
            0,
            nil,
            &dataSize,
            &name
        )
        
        guard status == noErr, let deviceName = name else {
            return nil
        }
        
        return deviceName as String
    }
    
    private func setupAudioUnit() throws {
        // 创建 Audio Component Description
        var componentDescription = AudioComponentDescription(
            componentType: kAudioUnitType_Output,
            componentSubType: kAudioUnitSubType_HALOutput,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0,
            componentFlagsMask: 0
        )
        
        guard let component = AudioComponentFindNext(nil, &componentDescription) else {
            throw AudioCaptureError.componentNotFound
        }
        
        var unit: AudioComponentInstance?
        var status = AudioComponentInstanceNew(component, &unit)
        guard status == noErr, let audioUnit = unit else {
            throw AudioCaptureError.instanceCreationFailed(status)
        }
        
        self.audioUnit = audioUnit
        
        // 启用输入
        var enableInput: UInt32 = 1
        status = AudioUnitSetProperty(
            audioUnit,
            kAudioOutputUnitProperty_EnableIO,
            kAudioUnitScope_Input,
            1, // Input element
            &enableInput,
            UInt32(MemoryLayout<UInt32>.size)
        )
        guard status == noErr else {
            throw AudioCaptureError.propertySetFailed(status)
        }
        
        // 禁用输出
        var disableOutput: UInt32 = 0
        status = AudioUnitSetProperty(
            audioUnit,
            kAudioOutputUnitProperty_EnableIO,
            kAudioUnitScope_Output,
            0, // Output element
            &disableOutput,
            UInt32(MemoryLayout<UInt32>.size)
        )
        guard status == noErr else {
            throw AudioCaptureError.propertySetFailed(status)
        }
        
        // 设置输入设备为 BlackHole
        var deviceID = blackHoleDeviceID
        status = AudioUnitSetProperty(
            audioUnit,
            kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global,
            0,
            &deviceID,
            UInt32(MemoryLayout<AudioDeviceID>.size)
        )
        guard status == noErr else {
            throw AudioCaptureError.propertySetFailed(status)
        }
        
        // 获取设备的原生格式，然后设置兼容的格式
        var deviceFormat = AudioStreamBasicDescription()
        var formatSize = UInt32(MemoryLayout<AudioStreamBasicDescription>.size)
        
        status = AudioUnitGetProperty(
            audioUnit,
            kAudioUnitProperty_StreamFormat,
            kAudioUnitScope_Input,
            1,
            &deviceFormat,
            &formatSize
        )
        
        if status == noErr {
            print("Device native format: \(deviceFormat.mSampleRate) Hz, \(deviceFormat.mChannelsPerFrame) channels")
            self.sampleRate = deviceFormat.mSampleRate
            self.channelCount = deviceFormat.mChannelsPerFrame
        }
        
        // 设置输出格式（使用 interleaved float 格式，更兼容）
        var audioFormat = AudioStreamBasicDescription(
            mSampleRate: sampleRate,
            mFormatID: kAudioFormatLinearPCM,
            mFormatFlags: kAudioFormatFlagIsFloat | kAudioFormatFlagIsPacked,
            mBytesPerPacket: 4 * channelCount,
            mFramesPerPacket: 1,
            mBytesPerFrame: 4 * channelCount,
            mChannelsPerFrame: channelCount,
            mBitsPerChannel: 32,
            mReserved: 0
        )
        
        status = AudioUnitSetProperty(
            audioUnit,
            kAudioUnitProperty_StreamFormat,
            kAudioUnitScope_Output,
            1, // Input element's output scope
            &audioFormat,
            UInt32(MemoryLayout<AudioStreamBasicDescription>.size)
        )
        guard status == noErr else {
            throw AudioCaptureError.propertySetFailed(status)
        }
        
        // 设置回调
        var callbackStruct = AURenderCallbackStruct(
            inputProc: inputCallback,
            inputProcRefCon: Unmanaged.passUnretained(self).toOpaque()
        )
        
        status = AudioUnitSetProperty(
            audioUnit,
            kAudioOutputUnitProperty_SetInputCallback,
            kAudioUnitScope_Global,
            0,
            &callbackStruct,
            UInt32(MemoryLayout<AURenderCallbackStruct>.size)
        )
        guard status == noErr else {
            throw AudioCaptureError.propertySetFailed(status)
        }
        
        // 初始化 Audio Unit
        status = AudioUnitInitialize(audioUnit)
        guard status == noErr else {
            throw AudioCaptureError.initializationFailed(status)
        }
        
        print("Audio Unit initialized successfully")
    }
}

// MARK: - Audio Callback

private func inputCallback(
    inRefCon: UnsafeMutableRawPointer,
    ioActionFlags: UnsafeMutablePointer<AudioUnitRenderActionFlags>,
    inTimeStamp: UnsafePointer<AudioTimeStamp>,
    inBusNumber: UInt32,
    inNumberFrames: UInt32,
    ioData: UnsafeMutablePointer<AudioBufferList>?
) -> OSStatus {
    
    let capture = Unmanaged<BlackHoleAudioCapture>.fromOpaque(inRefCon).takeUnretainedValue()
    
    // 准备缓冲区 (interleaved 格式)
    let channelCount = capture.channelCount
    let bytesPerFrame = 4 * channelCount
    let bufferSize = Int(inNumberFrames * bytesPerFrame)
    
    let buffer = UnsafeMutablePointer<Float>.allocate(capacity: bufferSize / 4)
    defer {
        buffer.deallocate()
    }
    
    var bufferList = AudioBufferList(
        mNumberBuffers: 1,
        mBuffers: AudioBuffer(
            mNumberChannels: channelCount,
            mDataByteSize: UInt32(bufferSize),
            mData: UnsafeMutableRawPointer(buffer)
        )
    )
    
    // 渲染音频数据
    guard let audioUnit = capture.audioUnit else {
        return noErr
    }
    
    let status = AudioUnitRender(
        audioUnit,
        ioActionFlags,
        inTimeStamp,
        inBusNumber,
        inNumberFrames,
        &bufferList
    )
    
    if status == noErr {
        // 调用回调传递音频数据
        capture.onAudioData?(buffer, inNumberFrames)
    } else if status != noErr {
        // 只在非静音错误时打印
        if status != -66635 { // kAudioUnitErr_NoConnection
            print("AudioUnitRender error: \(status)")
        }
    }
    
    return status
}

// MARK: - Error Types

enum AudioCaptureError: Error, LocalizedError {
    case deviceNotFound
    case componentNotFound
    case instanceCreationFailed(OSStatus)
    case propertySetFailed(OSStatus)
    case initializationFailed(OSStatus)
    case startFailed(OSStatus)
    
    var errorDescription: String? {
        switch self {
        case .deviceNotFound:
            return "BlackHole 2ch device not found. Please install BlackHole first."
        case .componentNotFound:
            return "Audio component not found"
        case .instanceCreationFailed(let status):
            return "Failed to create audio unit instance: \(status)"
        case .propertySetFailed(let status):
            return "Failed to set audio unit property: \(status)"
        case .initializationFailed(let status):
            return "Failed to initialize audio unit: \(status)"
        case .startFailed(let status):
            return "Failed to start audio capture: \(status)"
        }
    }
}
