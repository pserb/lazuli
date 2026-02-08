/**
 * Lazuli Web UI - Main Application
 * Coordinates controls, WebSocket, and preview rendering
 */

// Debounce timer for preview requests
let previewTimeout = null;
let isRendering = false;
let pendingRequest = null;

// Initialize application
document.addEventListener('DOMContentLoaded', async () => {
    // Initialize controls
    await initControls();
    
    // Connect WebSocket
    socket.onConnect(() => {
        console.log('Connected to server');
        // Request initial preview
        requestPreview();
    });
    
    socket.onMessage((data) => {
        handleServerMessage(data);
    });
    
    socket.onDisconnect(() => {
        console.log('Disconnected from server');
        setLoading(false);
    });
    
    socket.connect();
});

// Handle server messages
function handleServerMessage(data) {
    switch (data.status) {
        case 'Rendering':
            isRendering = true;
            setLoading(true);
            break;
            
        case 'Complete':
            isRendering = false;
            setLoading(false);
            
            if (data.image) {
                displayImage(data.image);
            }
            
            if (data.command) {
                document.getElementById('cli-command').textContent = data.command;
            }
            
            // Process pending request if any
            if (pendingRequest) {
                const req = pendingRequest;
                pendingRequest = null;
                sendRenderRequest(req);
            }
            break;
            
        case 'Error':
            isRendering = false;
            setLoading(false);
            showToast(data.error || 'Rendering failed', 'error');
            break;
    }
}

// Request a preview render
function requestPreview() {
    // Debounce: clear existing timeout
    if (previewTimeout) {
        clearTimeout(previewTimeout);
    }
    
    // Wait 100ms then request
    previewTimeout = setTimeout(() => {
        const request = {
            style: state.style,
            palette: state.palette,
            seed: state.seed,
            freq: state.freq,
            invert: state.invert,
            effects: state.effects
        };
        
        sendRenderRequest(request);
    }, 100);
}

// Send render request to server
function sendRenderRequest(request) {
    if (isRendering) {
        // Queue the request for after current render
        pendingRequest = request;
        return;
    }
    
    socket.send(request);
}

// Utility function for formatting effect strings
function formatEffectString(effectDef) {
    // API returns effectParams, not params
    const params = effectDef.effectParams || [];
    if (params.length === 0) {
        return effectDef.effectName;
    }
    
    const paramValues = params.map(p => {
        // API uses paramDefault, paramMin, paramMax
        const def = p.paramDefault !== null && p.paramDefault !== undefined 
            ? p.paramDefault 
            : ((p.paramMin + p.paramMax) / 2);
        return def;
    });
    
    return `${effectDef.effectName}:${paramValues.join(':')}`;
}

// Make functions available globally
window.requestPreview = requestPreview;
